drop table if exists logs.freetrial;
create table logs.freetrial (
    queued_at                timestamp       not null
  , status                   varchar(255)    not null
  , api_response_message     varchar(255)    default null
  , website_id               bigint          not null
  , selector_name            varchar(255)    default null
) distkey(website_id)
  sortkey(website_id, queued_at)
;
