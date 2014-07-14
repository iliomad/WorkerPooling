-module(file_echo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(NAMED_CHILD(I, N, Type), {I, {I, start_link, [{local, N}]}, permanent, 5000, Type, [I]}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, Pools} = application:get_env(file_echo, pools),
	PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [
        	{name, {local, Name}},
           	{worker_module, file_echo_worker}] ++ SizeArgs,
           	poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    	end, Pools),

	{ok, NumReaders} = application:get_env(file_echo, num_readers),


    {ok, {{one_for_one, 10, 10}, PoolSpecs ++ [?NAMED_CHILD(file_reader, file_reader, worker), ?NAMED_CHILD(file_echo_worker, file_echo_worker, worker)]}}.




