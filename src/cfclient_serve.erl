-module(cfclient_serve).

-export([encode/1]).

-export_type([cfclient_serve/0]).

-type cfclient_serve() ::
    #{ 'distribution' => cfclient_distribution:cfclient_distribution(),
       'variation' => binary()
     }.

encode(#{ 'distribution' := Distribution,
          'variation' := Variation
        }) ->
    #{ 'distribution' => Distribution,
       'variation' => Variation
     }.
