drop table if exists odi.website;

create table odi.website (
    website_id                        integer       not null
  , customer_id                       integer       default null
  , package_id                        varchar(20)   default null
  , cooperation_id                    varchar(20)   default null
  , reseller_id                       integer       default null
  , referrer_affiliate_id             integer       default null
  , subtype_id                        integer       default null
  , template_id                       integer       default null
  , template_type                     varchar(20)   default null
  , user_login                        varchar(1024) default null
  , user_email                        varchar(1024) default null
  , user_email_alternative            varchar(1024) default null
  , user_fullname                     varchar(max)  default null
  , user_birthdate                    varchar(1024) default null
  , user_address                      varchar(max)  default null
  , user_city                         varchar(max)  default null
  , user_country                      char(2)       default null
  , site_url                          varchar(1024) default null
  , site_currency                     char(3)       default null
  , site_language                     char(5)       default null
  , site_cms_server_id                integer       default null
  , site_web_server_id                integer       default null
  , is_notification_newsletter_active boolean       default null
  , is_privacy_policy_accepted        boolean       default null
  , is_listed_in_directory            boolean       default null
  , is_active                         boolean       default null
  , is_visible                        boolean       default null
  , is_blocked                        varchar(20)   default null
  , created_at                        timestamp     default null
  , google_analytics_activated_at     timestamp     default null
  , last_login_at                     timestamp     default null
  , last_upgrade_popup_at             timestamp     default null
  , updated_at                        timestamp     default null
  , obfuscated_website_id             char(17)      not null
  , primary key (website_id)
) distkey (website_id)
  sortkey (website_id, customer_id, package_id, cooperation_id, reseller_id, referrer_affiliate_id,
           subtype_id, template_id, user_email, user_email_alternative, user_country, site_url,
           site_currency, site_language, is_active, is_visible, is_blocked, created_at,
           last_login_at)
;
