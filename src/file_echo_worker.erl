-module(file_echo_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

%%% Module interface
-export([start_link/1, echo_line/3, stop/1]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


%%% Implementation of module interface functions
start_link(_Args) ->
	gen_server:start_link(?MODULE, [], []).

echo_line(Pid, Filename, Line) ->
	gen_server:call(Pid, {echo_line, Filename, Line}).

stop(Pid) ->
	gen_server:call(Pid, terminate).


%%% gen_server callback functions
init(_Args) -> {ok, "Initialised!"}.

handle_call({echo_line, Filename, Line}, _From, State) ->
	timer:sleep(1000),
	{ok, Handle} = file:open(Filename, [append]),
	file:write(Handle, Line),
	file:close(Handle),
	{reply, "Line echoed", State};
handle_call(terminate, _From, State) ->
	{stop, normal, ok, State}.


handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_Old, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.



