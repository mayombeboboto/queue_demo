-define(USERNAME,    application:get_env(queue_demo, username, <<"bob_admin">>)).
-define(PASSWORD,    application:get_env(queue_demo, password, <<"Ieksy78#u1">>)).
-define(QUEUE,       application:get_env(queue_demo, queue,    <<"queue_demo_queue">>)).
-define(EXCHANGE,    application:get_env(queue_demo, exchange, <<"queue_demo_exchange">>)).
-define(ROUTING_KEY, application:get_env(queue_demo, routing_key, <<"queue_demo_routing_key">>)).

-define(CONTENT_TYPE, <<"application/json">>).

-define(DB_QUEUE,            "queue_demo").
-define(DB_INIT_PATH,        "priv/db/init.sql").
-define(DB_TRANSACTION_PATH, "priv/db/transaction.sql").

-define(DB_USERNAME,  application:get_env(queue_demo, db_username, <<"postgres">>)).
-define(DB_PASSWORD,  application:get_env(queue_demo, db_password, <<"admin@123">>)).

