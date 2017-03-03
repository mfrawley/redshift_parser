/**
 * There exist website_ids in this table that don't have a valid reference in ods.website
 * The most plausible reason is that these were manually deleted from the DB. However,
 * there should be a reference in mgmt.systemjob which has been deleted on 2015-11-25 but
 * has a backup in s3://jimdo-backup/mgmt.systemjob/systemjob.gz (AWS data account)
 *
 * dwh=# select count(distinct a.website_id) from logs.activity a left join ods.website w using(website_id) where w.website_id is null;
 *  count
 * -------
 *  38577
 * (1 row)
 */
create table logs.activity(
    id                      bigint    not null     encode delta
  , website_id              integer   not null     encode raw
  , page_id                 bigint    default null encode mostly32
  , element_id              bigint    default null encode mostly32
  , activity_id             smallint  default null encode bytedict
  , created_at              timestamp default null encode raw
  , created_at_berlin_time  timestamp default null encode raw
  , primary key (id)
) distkey (website_id)
  sortkey (id, website_id, page_id, element_id, activity_id, created_at, created_at_berlin_time)
;
