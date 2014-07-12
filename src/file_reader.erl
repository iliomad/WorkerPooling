-module(file_reader).
-behaviour(gen_server).

%%% Module interface
-export([start_link/0, echo_file/3, stop/1]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


%%% Implementation of module interface functions
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

echo_file(Pid, Filename, WorkerId) ->
	gen_server:call(Pid, {echo_file, Filename, WorkerId}).

stop(Pid) ->
	gen_server:call(Pid, terminate).


%%% gen_server callback functions
init([]) -> {ok, ok}.

handle_call({echo_file, Filename, WorkerId}, _From, State) ->
	{ok, Handle} = file:open(Filename, [read]),
	{ok, LinesReadCount} = read_and_echo_line(Handle, WorkerId, 0),
	file:close(Handle),
	{reply, LinesReadCount, State};
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


%%% Private functions
read_and_echo_line(FileHandle, WorkerId, LinesReadCount) ->
	case file:read_line(FileHandle) of 
		{ok, Data} -> 
			file_echo_worker:echo_line(WorkerId, integer_to_list(LinesReadCount+1) ++"_output.txt", Data),
			read_and_echo_line(FileHandle, WorkerId, LinesReadCount+1);
		eof ->
			{ok, LinesReadCount};
		_ ->
			{error, "Something's not right"}
	end.

