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
    SupFlags = #{ strategy => rest_for_one,
                  intensity => 0,
                  period => 3600 },
    ChildMods = [queue_demo_db, queue_demo_server, queue_demo_consumer],
    ChildSpecs = [set_child_spec(Mod) || Mod <- ChildMods],

    {ok, {SupFlags, ChildSpecs}}.

set_child_spec(Mod) ->
    #{ id => Mod,
       start => {Mod, start_link, []},
       restart => permanent,
       shutdown => 5000,
       type => worker,
       modules => [Mod] }.
