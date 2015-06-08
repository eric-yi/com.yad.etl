%
% yi_xiaobin@163.com
% 2015-06-03 Eric

%% configuration
-record(config,
        {etc=""}).

-record(xml_rules,{
          datasource = {
            datasources, [],
            [{datasource, [{id, type}], [
              {parameter, [{name, value}], []}
            ]}]
          },

          etl = {
            etl, [],
            [{datasource, [{url}], []}]
          }

        }
      ).

-record(url, {
          scheme,
          user,
          host,
          port,
          path,
          parameter,
          frag,
          raw
        }).

