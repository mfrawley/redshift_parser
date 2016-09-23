create table mapping.activity(
  , activity_id     int           not null
   activity_name   varchar(255)  not null
  , primary key(activity_id)
  , unique(activity_name)
) diststyle all
  sortkey (activity_id)
;
drop table if exists mapping.activity;
