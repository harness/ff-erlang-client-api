-module(cfapi_get_evaluations_200_response).

-export([encode/1]).

-export_type([cfapi_get_evaluations_200_response/0]).

-type cfapi_get_evaluations_200_response() :: #{
                                              version => integer(),
                                              pageCount := integer(),
                                              itemCount := integer(),
                                              pageSize := integer(),
                                              pageIndex := integer()
                                            }.

encode(
  #{
    version := Version,
    pageCount := PageCount,
    itemCount := ItemCount,
    pageSize := PageSize,
    pageIndex := PageIndex
  }
) ->
  #{
    version => Version,
    pageCount => PageCount,
    itemCount => ItemCount,
    pageSize => PageSize,
    pageIndex => PageIndex
  }.
