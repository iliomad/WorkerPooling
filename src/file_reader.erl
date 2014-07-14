-module(file_reader).
-behaviour(gen_server).

%%% Module interface
-export([start_link/1, echo_file/3, stop/1]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


%%% Implementation of module interface functions
start_link(Args) ->
	case Args of 
		{local, ServerName} ->
			gen_server:start_link({local, ServerName}, ?MODULE, [ServerName], []);
		_ -> 
			gen_server:start_link(?MODULE, [], [])
	end.

echo_file(Pid, Filename, Mode) ->
	gen_server:call(Pid, {echo_file, Filename, Mode}).

stop(Pid) ->
	gen_server:call(Pid, terminate).


%%% gen_server callback functions
init([ServerName]) -> 
	{ok, ServerName}.

handle_call({echo_file, FileName, Mode}, _From, ServerName) ->
	OutFileName = atom_to_list(ServerName) ++ ".out",
	{ok, Handle} = file:open(FileName, [read]),
	{ok, LinesReadCount} = read_and_echo_lines(OutFileName, Handle, 0, Mode),
	file:close(Handle),
	{reply, LinesReadCount, ServerName};
handle_call(terminate, _From, ServerName) ->
	{stop, normal, ok, ServerName}.


handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_Old, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

read_and_echo_lines(OutFileName, FileHandle, LinesReadCount, Mode) ->
	case file:read_line(FileHandle) of
		{ok, Line} ->
			case Mode of 
				sequential ->
					sequential_echo(OutFileName, Line);
				parallel ->
					parallel_echo(OutFileName, Line);
				pooled ->
					pooled_echo(OutFileName, Line)
			end,
			read_and_echo_lines(OutFileName, FileHandle, LinesReadCount+1, Mode);
		eof ->
			{ok, LinesReadCount};
		_ ->
			{error, "Something's not right"}
	end.			



sequential_echo(OutFileName, Line) ->
	% Assumes a process named file_echo_worker is up and running
	file_echo_worker:echo_line(file_echo_worker, OutFileName, Line).


parallel_echo(OutFileName, Line) ->	
	% Parallel Stuff. I have to start the worker processes on the fly. 
	% That leaves them unsupervised.....but does that matter? 
	% If a process dies, is there any need to restart it?
	% Declaring a fun and just outputting the line is easy. 
	% Firing up a gen_server (file_echo_worker) and invoking echo_line seems unnecessary.
	spawn(fun() ->
			timer:sleep(1000),
			{ok, Handle} = file:open(OutFileName, [append]),
			file:write(Handle, Line),
			file:close(Handle)
		end
	).


pooled_echo(OutFileName, Line) ->
	poolboy:transaction(dapool, fun(Worker) ->
		gen_server:call(Worker, {echo_line, OutFileName, Line})
	end).
