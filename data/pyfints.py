from fints.client import FinTS3PinTanClient, NeedTANResponse
from mt940.models import Date
import base64
import hashlib
import json
import os
import sys
import tempfile


def send(message_type, **values):
    print(json.dumps({"type": message_type, **values}), flush=True)


def receive(expected_type):
    line = sys.stdin.readline()
    if not line:
        raise RuntimeError("The fints2ledger process closed the protocol input")
    message = json.loads(line)
    if message.get("type") != expected_type:
        raise RuntimeError(
            f"Expected protocol message '{expected_type}', got {message.get('type')!r}"
        )
    return message


def state_path(args):
    identity = "\0".join((args["endpoint"], args["blz"], args["account"]))
    digest = hashlib.sha256(identity.encode()).hexdigest()[:24]
    return os.path.join(args["stateDirectory"], f"fints-state-{digest}.bin")


def load_state(path):
    try:
        with open(path, "rb") as reader:
            return reader.read()
    except FileNotFoundError:
        return None


def store_state(path, data):
    os.makedirs(os.path.dirname(path), mode=0o700, exist_ok=True)
    fd, temporary_path = tempfile.mkstemp(dir=os.path.dirname(path))
    try:
        os.fchmod(fd, 0o600)
        with os.fdopen(fd, "wb") as writer:
            writer.write(data)
        os.replace(temporary_path, path)
        os.chmod(path, 0o600)
    except BaseException:
        try:
            os.unlink(temporary_path)
        except FileNotFoundError:
            pass
        raise


def choose_tan_settings(client):
    if not client.get_current_tan_mechanism():
        client.fetch_tan_mechanisms()
        mechanisms = list(client.get_tan_mechanisms().items())
        if not mechanisms:
            # TAN-less banks do not advertise any mechanisms. There is no
            # mechanism or medium to select, so continue with the dialog.
            return
        if len(mechanisms) == 1:
            client.set_tan_mechanism(mechanisms[0][0])
        else:
            send(
                "tan_methods",
                methods=[
                    {"id": identifier, "name": mechanism.name}
                    for identifier, mechanism in mechanisms
                ],
            )
            selected = receive("tan_method")["id"]
            if selected not in dict(mechanisms):
                raise RuntimeError(f"Unknown TAN method selected: {selected!r}")
            client.set_tan_mechanism(selected)

    if client.selected_tan_medium is None and client.is_tan_media_required():
        media = client.get_tan_media()[1]
        if len(media) == 1:
            client.set_tan_medium(media[0])
        elif len(media) == 0:
            # Some banks demand a medium field but return no selectable media.
            client.selected_tan_medium = ""
        else:
            send(
                "tan_media",
                media=[
                    {
                        "index": index,
                        "name": medium.tan_medium_name,
                        "mobile": medium.mobile_number_masked,
                    }
                    for index, medium in enumerate(media)
                ],
            )
            selected = receive("tan_medium")["index"]
            if not isinstance(selected, int) or not 0 <= selected < len(media):
                raise RuntimeError(f"Unknown TAN medium selected: {selected!r}")
            client.set_tan_medium(media[selected])


def send_tan(client, response):
    while isinstance(response, NeedTANResponse):
        matrix = None
        if response.challenge_matrix:
            mime_type, image_data = response.challenge_matrix
            matrix = {
                "mimeType": mime_type,
                "data": base64.b16encode(image_data).decode("ascii"),
            }

        send(
            "tan_challenge",
            challenge=response.challenge or "A TAN is required",
            decoupled=bool(response.decoupled),
            hhduc=response.challenge_hhduc,
            matrix=matrix,
        )
        tan = receive("tan")["value"]
        if not isinstance(tan, str):
            raise RuntimeError("The TAN must be a string")
        response = client.send_tan(response, tan)
    return response


class TransactionRetriever:
    def __init__(self, client, account_number):
        self.client = client
        self.account_number = account_number

    def get_transactions(self, start_date, end_date):
        accounts = self.client.get_sepa_accounts()
        account = self.find_matching_account(accounts)
        return self.client.get_transactions(account, start_date, end_date)

    def find_matching_account(self, accounts):
        for account in accounts:
            if account.accountnumber == self.account_number:
                return account
        raise RuntimeError(
            f"Could not find account '{self.account_number}'. Possible accounts: {accounts}"
        )


DATE_FORMAT = "%Y/%m/%d"


def transaction_to_object(transaction):
    hbci_data = transaction.data
    posting_text = hbci_data.get(
        "posting_text", hbci_data.get("AdditionalEntryInformation")
    )
    return {
        "date": hbci_data["date"].strftime(DATE_FORMAT),
        "amount": str(hbci_data["amount"].amount),
        "currency": hbci_data["amount"].currency,
        "posting": (posting_text or "").strip(),
        "payee": (hbci_data["applicant_name"] or "").strip(),
        "purpose": (hbci_data["purpose"] or "").strip(),
    }


def date_string_to_mt940_date(date_string):
    year, month, day = date_string.split("/")
    return Date(year=year, month=month, day=day)


def run(args):
    path = state_path(args)
    client = FinTS3PinTanClient(
        args["blz"],
        args["account"],
        args["password"],
        args["endpoint"],
        product_id="EC449295201FA9BE5040B9154",
        from_data=load_state(path),
    )
    choose_tan_settings(client)

    with client:
        client.init_tan_response = send_tan(client, client.init_tan_response)
        result = TransactionRetriever(client, args["selectedAccount"]).get_transactions(
            date_string_to_mt940_date(args["start"]),
            date_string_to_mt940_date(args["end"]),
        )
        result = send_tan(client, result)
        transactions = [transaction_to_object(transaction) for transaction in result]

    store_state(path, client.deconstruct(including_private=True))
    send("transactions", transactions=transactions)


def main():
    try:
        run(receive("start")["arguments"])
    except Exception as exception:
        send("error", message=str(exception))
        raise


if __name__ == "__main__":
    main()
