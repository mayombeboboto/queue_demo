-- :create_transactions_table
CREATE TABLE transactions (
    trans_id BIGSERIAL,
    trans_amount INT,
    trans_timestamp TIMESTAMP
);

-- :drop_transactions_table
DROP TABLE transactions;

