-module(queue_demo_server).
%%----------------------------------------------------------%%
-behavior(gen_server).
%%----------------------------------------------------------%%
-export([start_link/0]).
-export([get_channel/0]).
-export([publish_msg/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
%%----------------------------------------------------------%%
-include("include/queue_demo.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
%%----------------------------------------------------------%%
-type encoded_json() :: binary().
%%----------------------------------------------------------%%
-record(state, { connection :: pid(),
                 channel :: pid() }).
%%----------------------------------------------------------%%
%%------------------------ APIs ----------------------------%%
%%----------------------------------------------------------%%
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_channel() -> pid().
get_channel() ->
    gen_server:call(?MODULE, channel).

-spec publish_msg(encoded_json()) -> ok.
publish_msg(Payload) ->
    gen_server:cast(?MODULE, {payload, Payload}).

%%----------------------------------------------------------%%
%%------------------- CALLBACK FUNCTIONS -------------------%%
%%----------------------------------------------------------%%
init([]) ->
    process_flag(trap_exit, true),
    {ok, Connection} = start_connection(),
    {ok, Channel} = open_channel(Connection),

    declare_exchange(Channel),
    declare_queue(Channel),
    bind_queue(Channel),

    PublishPid = whereis(queue_demo),
    amqp_channel:register_return_handler(Channel, PublishPid),
    {ok, #state{ connection=Connection,
                 channel=Channel }}.

handle_call(channel, _From, State=#state{ channel=Channel }) ->
    {reply, Channel, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({payload, Payload}, State=#state{ channel=Channel }) ->
    Publish = #'basic.publish'{ exchange = ?EXCHANGE,
                                routing_key = ?ROUTING_KEY,
                                mandatory = true },
    BasicProps = #'P_basic'{ content_type = ?CONTENT_TYPE,
                             message_id = generate_id() },

    Msg = #amqp_msg{ payload = Payload,
                     props = BasicProps },
    amqp_channel:cast(Channel, Publish, Msg),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State=#state{}) ->
    amqp_channel:close(State#state.channel),
    amqp_connection:close(State#state.connection),
    ok.
%%----------------------------------------------------------%%
start_connection() ->
    {ok, Connection} = amqp_connection:start(set_network_params()),
    {ok, Connection}.

open_channel(Connection) ->
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {ok, Channel}.

declare_exchange(Channel) ->
    Declare = #'exchange.declare'{ exchange = ?EXCHANGE },
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare).

declare_queue(Channel) ->
    Declare = #'queue.declare'{ queue = ?QUEUE },
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare).

bind_queue(Channel) ->
    Binding = #'queue.bind'{ queue = ?QUEUE,
                             exchange = ?EXCHANGE,
                             routing_key = ?ROUTING_KEY },
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding).
%%----------------------------------------------------------%%
set_network_params() ->
    #amqp_params_network{ username = ?USERNAME,
                          password = ?PASSWORD }.

generate_id() ->
    Bits = entropy_string:bits(5.0e6, 1.0e12),
    entropy_string:random_string(Bits).