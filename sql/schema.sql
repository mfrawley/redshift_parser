create table mapping.activity(
   activity_name   varchar(255)  not null
  , activity_id     int           not null
  , primary key(activity_id)
  , unique(activity_name)
) diststyle all
  sortkey (activity_id)
;
drop table if exists mapping.activity;
