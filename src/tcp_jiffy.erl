-module(tcp_jiffy).
-behaviour(application).
-behaviour(supervisor).

%% Console APIs
-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> application:start(?MODULE).
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() -> application:stop(?MODULE).
stop(_State) -> ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
