-module(queue_demo_consumer).
%%----------------------------------------------------------%%
-behaviour(gen_server).
%%----------------------------------------------------------%%
-export([start_link/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
%%----------------------------------------------------------%%
-include("include/queue_demo.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
%%----------------------------------------------------------%%
-record(state, { channel :: pid(),
                 tag :: binary() }).
%%----------------------------------------------------------%%
%%------------------------ APIs ----------------------------%%
%%----------------------------------------------------------%%
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%----------------------------------------------------------%%
%%------------------- CALLBACK FUNCTIONS -------------------%%
%%----------------------------------------------------------%%
init([]) ->
    Channel = queue_demo_server:get_channel(),
    BasicConsume = #'basic.consume' { queue = ?QUEUE },
    amqp_channel:subscribe(Channel, BasicConsume, self()),
    {ok, #state{ channel=Channel }}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#'basic.consume_ok'{ consumer_tag = Tag }, State) ->
    {noreply, State#state{ tag=Tag }};
handle_info({#'basic.deliver'{}, #'amqp_msg'{ payload=Payload }}, State) ->
    {ok, Amount} = extract_field_from_payload(<<"amount">>, 0, Payload),
    queue_demo_db:insert_transaction(Amount),
    {noreply, State};
handle_info(Msg, State) ->
    io:format("Msg: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, #state{ channel=Channel, tag=Tag }) ->
    amqp_channel:call(Channel, #'basic.cancel'{ consumer_tag = Tag }).

%%----------------------------------------------------------%%
extract_field_from_payload(Field, Default, Payload) ->
    DecodedPayload = jsx:decode(Payload),
    Value = maps:get(Field, DecodedPayload, Default),
    {ok, Value}.

