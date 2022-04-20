-- :insert_transaction
INSERT INTO transactions (trans_amount, trans_timestamp)
VALUES ($1, $2);