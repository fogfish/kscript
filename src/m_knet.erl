%% @doc
%%   Networking I/O
-module(m_knet).
-compile({parse_transform, category}).

%% monad interface
-export([unit/1, fail/1, '>>='/2]).

%% BDD interface
-export([
   %% Given
   given/0,
   url/1,

   %% With/When ()
   with/0,
   method/1, x/1,
   header/2, h/2, h/1, 

   %% Then
   then/0,
   require/2
]).

%%%----------------------------------------------------------------------------   
%%%
%%% monad interface
%%%
%%%----------------------------------------------------------------------------

unit(X) ->
   m_state:unit(X).

fail(X) ->
   m_state:fail(X).

'>>='(X, Fun) ->
   m_state:'>>='(X, Fun).

%%%----------------------------------------------------------------------------   
%%%
%%% Given
%%%
%%%----------------------------------------------------------------------------
given() -> 
   m_state:unit(undefined).

url(Url) ->
   m_http:new(Url).

%%%----------------------------------------------------------------------------   
%%%
%%% When
%%%
%%%----------------------------------------------------------------------------
with() -> 
   m_state:unit(undefined).


%%
%%
x(Mthd) ->
   m_http:x(Mthd).

method(Mthd) ->
   m_http:method(Mthd).

%%
%%
h(Head) ->
   m_http:h(Head).

h(Head, Value) ->
   m_http:h(Head, Value).

header(Head, Value) ->
   m_http:header(Head, Value).


%%%----------------------------------------------------------------------------   
%%%
%%% Then
%%%
%%%----------------------------------------------------------------------------
then() ->
   fun(State0) ->
      %% @todo get timeout from protocol parameters (with section)
      [Http | State1] = ( m_http:request() )(State0),
      {Status, Headers, Content} = kscript_codec:decode(Http),
      [Content |
         [identity ||
            lens:put(status(), Status, State1),
            lens:put(headers(), Headers, _),
            lens:put(content(), Content, _)
         ]
      ]
   end.


%%
%% lenses
status() -> lens:c(lens:map(http, #{}), lens:map(status,  none)).
headers() -> lens:c(lens:map(http, #{}), lens:map(headers,    [])).
content() -> lens:c(lens:map(http, #{}), lens:map(content,  none)).


%%
%%
require(status, Expect) ->
   require(status(), Expect);

require(Lens, Expect) ->
   fun(State) ->
      Expect = lens:get(Lens, State),
      [ok|State]
   end.



