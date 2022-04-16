%%%-------------------------------------------------------------------
%% @doc queue_demo public API
%% @end
%%%-------------------------------------------------------------------

-module(queue_demo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    queue_demo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
