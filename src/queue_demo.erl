-module(queue_demo).
%%----------------------------------------------------------%%
-behavior(gen_server).
%%----------------------------------------------------------%%
-export([start_link/0]).
-export([publish_msg/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
%%----------------------------------------------------------%%
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec publish_msg(#{}) -> no_return().
publish_msg(Payload) ->
    queue_demo_server:publish_msg(jsx:encode(Payload)).

%%----------------------------------------------------------%%
init([]) ->
    process_flag(trap_exit, true),
    {ok, []}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Msg: ~p~n", [Msg]),
    {noreply, [Msg|State]}.

terminate(_Reason, _State) ->
    ok.