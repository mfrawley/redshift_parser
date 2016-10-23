create table if not exists online_marketing.reporting (
    click_date                            date                not null
  , source                                varchar(255)        not null
  , campaign                              varchar(1024)       not null
  , country                               varchar(7)          not null
  , num_clicks                            bigint              not null
  , num_orders_last_click                 bigint              not null
  , num_signups_last_click                bigint              not null
  , num_orders_first_click                bigint              not null
  , num_signups_first_click               bigint              not null
  , sum_click_costs                       decimal(12, 2)      not null
  , sum_all_costs                         decimal(12, 2)      not null
  , sum_all_costs_brand                   decimal(12, 2)      not null
  , sum_all_costs_no_brand                decimal(12, 2)      not null
  , num_orders_am                         float               not null
);


drop table if exists online_marketing.om_mart_weights;
create table online_marketing.om_mart_weights (
    click_date                            date                not null
  , om_type                               varchar(255)        not null
  , attribution                           double precision
);
