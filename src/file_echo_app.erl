-module(file_echo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, echo_file/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, _Args) ->
    file_echo_sup:start_link().

stop(_State) ->
    ok.


echo_file(Filename, Mode) ->
	file_reader:echo_file(file_reader, Filename, Mode).

