from uuid import uuid4

from flask import Flask, jsonify, request
from textwrap import dedent
from blockchain import Blockchain, Block

node_identifier = str(uuid4()).replace('-', '')
app = Flask(__name__)
blockchain = Blockchain()


@app.route('/mine', methods=['GET'])
def mine():
    last_block: Block = blockchain.last_block
    last_proof: int = last_block.proof

    proof: int = Blockchain.proof_of_work(last_proof)
    blockchain.new_transaction(sender="0", recipient=node_identifier, amount=1)

    previous_hash = blockchain.hash(last_block)
    block = blockchain.new_block(proof=proof, previous_hash=previous_hash)

    response = {
        'message': "New Block Forged",
        'index': block.index,
        'transactions': [transaction.as_dict() for transaction in block.transactions],
        'proof': block.proof,
        'previous_hash': block.previous_hash
    }
    return jsonify(response), 200


@app.route('/transactions/new', methods=['POST'])
def new_transaction():
    values = request.get_json()
    required = ['sender', 'recipient', 'amount']
    if not all(k in values for k in required):
        return 'Missing values', 400

    index = blockchain.new_transaction(sender=values['sender'], recipient=values['recipient'], amount=values['amount'])

    response = {'message': f'Transaction will be added to block {index}'}

    return jsonify(response), 200


@app.route('/chain', methods=['GET'])
def full_chain():
    response = {
        'chain': [block.as_dict() for block in blockchain.chain],
        'length': len(blockchain.chain)
    }
    return jsonify(response), 200


@app.route('/nodes/register', methods=['POST'])
def register_nodes():
    values = request.get_json()

    nodes = values.get('nodes')
    if nodes is None:
        return "Error: Please supply a valid list of nodes", 400

    for node in nodes:
        blockchain.register_node(node)

    response = {
        'message': 'New nodes have been added',
        'total_nodes': list(blockchain.nodes)
    }

    return jsonify(response), 201

@app.route('/nodes/resolve', methods=['GET'])
def consensus():
    replaced = blockchain.resolve_conflicts()

    if replaced:
        response = {
            'message': 'Our chain was replaced',
            'new_chain': [block.as_dict() for block in blockchain.chain]
        }
    else:
        response = {
            'message': 'Our chain is authoritative',
            'chain': [block.as_dict() for block in blockchain.chain]
        }

    return jsonify(response), 200


if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000)