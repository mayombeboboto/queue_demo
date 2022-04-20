-module(queue_demo_db).
%%----------------------------------------------------------%%
-behaviour(gen_server).
%%----------------------------------------------------------%%
-export([start_link/0]).
-export([insert_transaction/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
%%----------------------------------------------------------%%
-include("include/queue_demo.hrl").
%%----------------------------------------------------------%%
-record(state, { connection :: pid(),
                 queries :: list()}).

%%----------------------------------------------------------%%
%%------------------------ APIs ----------------------------%%
%%----------------------------------------------------------%%
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec insert_transaction(non_neg_integer()) -> ok.
insert_transaction(Amount) ->
    gen_server:cast(?MODULE, {insert, Amount}).
%%----------------------------------------------------------%%
%%------------------- CALLBACK FUNCTIONS -------------------%%
%%----------------------------------------------------------%%
init([]) ->
    {ok, Connection} = create_connection(),
    {ok, Queries} = get_queries(init),
    ok = execute_query(Connection, Queries, create_transactions_table),

    link(Connection),
    {ok, #state{ connection = Connection,
                 queries = Queries }}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({insert, Amount}, State) ->
    {ok, Queries} = get_queries(transactions),
    Params = [Amount, calendar:local_time()],
    ok = execute_query(State#state.connection, Queries, insert_transaction, Params),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{ connection = Connection, queries = Queries}) ->
    execute_query(Connection, Queries, drop_transactions_table),
    epgsql:close(Connection).
%%----------------------------------------------------------%%
create_connection() ->
    Options = #{ host => "localhost",
                 username => ?DB_USERNAME,
                 password => ?DB_PASSWORD,
                 database => ?DB_QUEUE },
    {ok, Connection} = epgsql:connect(Options),
    {ok, Connection}.

get_queries(init) ->
    {ok, Queries} = eql:compile(?DB_INIT_PATH),
    {ok, Queries};
get_queries(transactions) ->
    {ok, Queries} = eql:compile(?DB_TRANSACTION_PATH),
    {ok, Queries}.

execute_query(Connection, Queries, QueryName) ->
    execute_query(Connection, Queries, QueryName, []).

execute_query(Connection, Queries, QueryName, []) ->
    {ok, Query} = eql:get_query(QueryName, Queries),
    epgsql:squery(Connection, binary_to_list(Query)),
    ok;
execute_query(Connection, Queries, QueryName, Params) ->
    {ok, Query} = eql:get_query(QueryName, Queries),
    io:format("Query: ~p~n", [Query]),
    io:format("Params: ~p~n", [Params]),
    io:format("Connection: ~p~n", [Connection]),
    epgsql:equery(Connection, binary_to_list(Query), Params),
    ok.