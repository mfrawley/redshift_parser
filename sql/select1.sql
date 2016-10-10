select
    fd.dim_date
  , fd.website_id
  , max(case when fsp.website_id is not null then 1 else 0 end) as has_active_subscription
  , max(item_charge_net_eur_monthly) as item_charge_net_eur_monthly
from blah_foo fd
left join _finance_service_period_monthly fsp
 on to_date(fd.dim_date, 'YYYYMM') between to_date(service_start_at, 'YYYYMM') and dateadd(month, -1, to_date(service_end_at, 'YYYYMM'))
and fd.website_id = fsp.website_id
group by 1,2
;
