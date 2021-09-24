from time import time
import hashlib
import json
from urllib.parse import urlparse

import requests as requests
from flask import jsonify


class Transaction(object):
    def __init__(self, sender: str, recipient: str, amount: int):
        self.sender = sender
        self.recipient = recipient
        self.amount = amount

    def as_dict(self):
        return {
            'sender': self.sender,
            'recipient': self.recipient,
            'amount': self.amount
        }

    def __str__(self):
        return jsonify(self.as_dict())


class Block(object):
    def __init__(self, index: int, transactions: list[Transaction], proof: int, previous_hash: str = None):
        self.index = index
        self.timestamp = time()
        self.transactions = transactions
        self.proof = proof
        self.previous_hash = previous_hash

    def __str__(self):
        return jsonify(self.as_dict())

    def as_dict(self):
        return {
            'index': self.index,
            'timestamp': self.timestamp,
            'transactions': [transaction.as_dict() for transaction in self.transactions],
            'proof': self.proof,
            'previous_hash': self.previous_hash
        }


class Blockchain(object):
    def __init__(self):
        self.chain: list[Block] = []
        self.current_transactions: list[Transaction] = []
        self.nodes: set[str] = set()
        self.new_block(previous_hash="1", proof=100)

    def register_node(self, address: str) -> None:
        parsed_url = urlparse(address)
        self.nodes.add(parsed_url.netloc)

    @staticmethod
    def proof_of_work(last_proof: int) -> int:
        proof = 0
        while not Blockchain.valid_proof(last_proof, proof):
            proof += 1

        return proof

    @staticmethod
    def valid_proof(last_proof: int, proof: int) -> bool:
        guess = f'{last_proof}{proof}'.encode()
        guess_hash: str = hashlib.sha256(guess).hexdigest()
        return guess_hash[:4] == "0000"

    @staticmethod
    def valid_chain(chain: list[Block]):
        last_block: Block = chain[0]
        current_index: int = 1

        while current_index < len(chain):
            block = chain[current_index]
            print(f'{last_block}')
            print(f'{block}')
            print("\n-----------\n")
            if block.previous_hash != Blockchain.hash(last_block):
                return False

            if not Blockchain.valid_proof(last_block.proof, block.proof):
                return False

            last_block = block
            current_index += 1

        return True

    def resolve_conflict(self):
        neighbours = self.nodes
        new_chain = None

        max_length = len(self.chain)

        for node in neighbours:
            response = requests.get(f'http://{node}/chain')

            if response.status_code == 200:
                length = response.json()['length']
                chain = response.json()['chain']

                if length > max_length and Blockchain.valid_chain(chain):
                    max_length = length
                    new_chain = chain

        if new_chain:
            self.chain = new_chain
            return True

        return False

    def new_block(self, proof: int, previous_hash: str = None) -> Block:
        block = Block(index=len(self.chain) + 1, transactions=self.current_transactions, proof=proof, previous_hash=previous_hash)

        self.current_transactions = []
        self.chain.append(block)
        return block

    def new_transaction(self, sender: str, recipient: str, amount: int) -> int:
        self.current_transactions.append(Transaction(sender, recipient, amount))
        return self.last_block.index + 1

    @staticmethod
    def hash(block: Block) -> str:
        block_string = json.dumps(block.as_dict(), sort_keys=True).encode()
        return hashlib.sha256(block_string).hexdigest()

    @property
    def last_block(self) -> Block:
        return self.chain[-1]

    def __str__(self):
        return str(self.last_block)


if __name__ == "__main__":
    tr = Transaction("a", "b", 5)

    blockchain = Blockchain()
    print(blockchain)

    blockchain.new_transaction("a", "b", 5)

    print(blockchain)
