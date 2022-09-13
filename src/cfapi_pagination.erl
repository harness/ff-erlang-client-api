-module(cfapi_pagination).

-export([encode/1]).

-export_type([cfapi_pagination/0]).

-type cfapi_pagination() ::
    #{ 'version' => integer(),
       'pageCount' := integer(),
       'itemCount' := integer(),
       'pageSize' := integer(),
       'pageIndex' := integer()
     }.

encode(#{ 'version' := Version,
          'pageCount' := PageCount,
          'itemCount' := ItemCount,
          'pageSize' := PageSize,
          'pageIndex' := PageIndex
        }) ->
    #{ 'version' => Version,
       'pageCount' => PageCount,
       'itemCount' => ItemCount,
       'pageSize' => PageSize,
       'pageIndex' => PageIndex
     }.
