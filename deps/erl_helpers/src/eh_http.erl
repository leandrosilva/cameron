%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Helper module for http operations with httpc and mochiweb_request modules.

-module(eh_http).
-author('Leandro Silva <leandrodoze@gmail.com>').

-export([http_post/2, http_get/1, http_put/2, http_delete/1]).
-export([http_response/2, http_response/3, http_response/4]).

%% External API

%%
%% HTTP Request functions
%%
%% These functions use httpc module.
%%

http_post(Uri, Body) ->
  http_request(post, Uri, Body).
  
http_get(Uri) ->
  http_request(get, Uri).

http_put(Uri, Body) ->
  http_request(put, Uri, Body).

http_delete(Uri) ->
  http_request(delete, Uri).

%%
%% HTTP Response functions
%%
%% These functions use mochiweb_request module.
%%
  
http_response(Request, Status) ->
  Request:respond({Status, [], []}).
  
http_response(Request, Status, Body) ->
  Request:respond({Status, [], Body}).
  
http_response(Request, Status, Headers, Body) ->
  Request:respond({Status, Headers, Body}).
  
%% Internal API

http_request(HttpMethod, Uri) ->
  httpc:request(HttpMethod, {Uri, []}, [], []).
  
http_request(HttpMethod, Uri, Body) ->
  Headers = [],
  ContentType = "application/json",
  HttpOptions = [],
  Options = [{body_format, string}],

  httpc:request(HttpMethod, {Uri, Headers, ContentType, Body}, HttpOptions, Options).
  