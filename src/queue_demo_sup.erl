%%----------------------------------------------------------%%
%% @doc queue_demo top level supervisor.
%% @end
%%----------------------------------------------------------%%
-module(queue_demo_sup).
%%----------------------------------------------------------%%
-behaviour(supervisor).
%%----------------------------------------------------------%%
-export([start_link/0]).

-export([init/1]).
%%----------------------------------------------------------%%
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Strategy = #{ strategy => rest_for_one,
                  intensity => 0,
                  period => 3600 },
    {ok, {Strategy, [set_child_spec(queue_demo_server)]}}.

set_child_spec(Mod) ->
    #{ id => Mod,
       start => {Mod, start_link, []},
       restart => permanent,
       shutdown => 5000,
       type => worker,
       modules => [Mod] }.
