library(sqldf)
cat("format_session.R - call data_structure.R for one hot encoding\n")
source('data_structure.R')

cat("format_session.R - Read sessions data\n")
sessions <- read.csv("sessions.csv",stringsAsFactors=FALSE)
sessions <- sessions[,c("user_id","action","secs_elapsed")]
uid <- unique(sessions$user_id)
write.csv(uid,"sessions_id.csv",row.names=FALSE)

cat("format_session.R  - Get Ids common in train & Sessions and test & Sessions\n")
train_id <- intersect(unique(train_data$id),sessions$user_id)
test_id <- intersect(unique(test_data$id),sessions$user_id)

train_data$session_available <- ifelse(train_data$id %in% train_id,1,0)
test_data$session_available <- ifelse(test_data$id %in% test_id,1,0)

cat("format_session.R - Get training & test set sessions\n")
sessions_train <- sessions[sessions$user_id %in% train_id,]
sessions_test <- sessions[sessions$user_id %in% test_id,]

cat("format_session.R - Remove full sessions data..\n")
rm(sessions)
gc(reset=TRUE)

action_list <- c('guest_booked_elsewhere',	'email_itinerary_colorbox',	'add_guests',	'respond',	'open_hard_fallback_modal',	'acculynk_bin_check_success',	'acculynk_load_pin_pad',	'acculynk_session_obtained',	'acculynk_pin_pad_inactive',	'airbrb',	'guest_billing_receipt',	'friend_listing',	'print_confirmation',	'signup_weibo',	'ajax_special_offer_dates_available',	'deauthorize',	'desks',	'envoy_bank_details_redirect',	'plaxo_cb',	'preapproval',	'toggle_availability',	'update_message',	'wishlists',	'receipt',	'requested',	'itinerary',	'onenight',	'ajax_price_and_availability',	'complete_status',	'change',	'pending',	'12',	'agree_terms_check',	'message_to_host_change',	'reservation',	'update_country_of_residence',	'slideshow',	'phone_verification_success',	'phone_verification_call_taking_too_long',	'message_to_host_focus',	'at_checkpoint',	'qt_with',	'ajax_photo_widget',	'load_more',	'multi_message',	'phone_verification_number_sucessfully_submitted',	'complete_redirect',	'travel_plans_current',	'concierge',	'pay',	'15',	'image_order',	'apply_coupon_click_success',	'endpoint_error',	'phone_verification_number_submitted_for_sms',	'feed',	'review_page',	'request_new_confirm_email',	'press_release',	'webcam_upload',	'verify',	'read_policy_click',	'agree_terms_uncheck',	'jumio_redirect',	'populate_from_facebook',	'ajax_send_message',	'qt_reply_v2',	'kba_update',	'this_hosting_reviews',	'ajax_photo_widget_form_iframe',	'qt2',	'push_notification_callback',	'apply_reservation',	'cancellation_policy_click',	'cancellation_policies',	'clear_reservation',	'multi_message_attributes',	'identity',	'terms',	'p4_refund_policy_terms',	'cancel',	'ajax_image_upload',	'toggle_starred_thread',	'phone_verification_number_submitted_for_call',	'new_session',	'jumio_token',	'ajax_google_translate_reviews',	'connect',	'impressions',	'kba',	'profile_pic',	'add_note',	'toggle_archived_thread',	'p4_terms',	'10',	'edit_verification',	'handle_vanity_url',	'delete',	'approve',	'travel',	'friends_new',	'languages_multiselect',	'update_hide_from_search_engines',	'status',	'references',	'phone_number_widget',	'relationship',	'edit',	'upload',	'phone_verification_phone_number_removed',	'coupon_code_click',	'reviews_new',	'11',	'apply_coupon_click',	'change_currency',	'coupon_field_focus',	'apply_coupon_error_type',	'apply_coupon_error',	'jumio',	'complete',	'my',	'glob',	'callback',	'update_friends_display',	'remove_dashboard_alert',	'other_hosting_reviews',	'show_personalize',	'ajax_referral_banner_type',	'ajax_google_translate',	'ajax_lwlb_contact',	'trust',	'dashboard',	'recommended_listings',	'confirm_email',	'phone_verification_error',	'ajax_get_referrals_amt',	'privacy',	'ask_question',	'update_notifications',	'transaction_history_paginated',	'tell_a_friend',	'calendar_tab_inner2',	'locale_from_host',	'message',	'photography_update',	'booking',	'views_campaign',	'ajax_check_dates',	'transaction_history',	'ajax_refresh_subtotal',	'ajax_referral_banner_experiment_type',	'account',	'login_modal',	'signature',	'ajax_get_results',	'salute',	'ajax_google_translate_description',	'mobile_oauth_callback',	'signup_login',	'similar_listings_v2',	'faq',	'update_cached',	'similar_listings',	'track_page_view',	'terms_and_conditions',	'personalize',	'faq_experiment_ids',	'become_user',	'hosting_social_proof',	'payment_methods',	'submit_contact',	'search_results',	'lookup',	'top_destinations',	'create',	'facebook_auto_login',	'index',	'about_us',	'destroy',	'signup_modal',	'open_graph_setting',	'forgot_password',	'recent_reservations',	'create_multiple',	'clickthrough',	'friends',	'other_hosting_reviews_first',	'supported',	'listings',	'contact_new',	'issue',	'header_userpic',	'decision_tree',	'authenticate',	'show',	'mobile_landing_page',	'email_wishlist',	'localization_settings',	'login',	'founders',	'hospitality_standards',	'travel_plans_previous',	'pending_tickets',	'redirect',	'social-media',	'populate_help_dropdown',	'request_photography',	'email_share',	'countries',	'settings',	'requirements',	'payment_instruments',	'faq_category',	'notification',	'rate')

action_list_1 <- c('spoken_languages',	'multi',	'listing',	'detect_fb_session',	'social',	'ajax_ldp',	'home_safety_landing',	'my_listings','locations',	'click',	'available',	'payout_preferences',	'signed_out_modal',	'set_password',	'guarantee',	'patch',	'photography',	'campaigns',	'change_password',	'authorize',	'unavailabilities',	'press_news',	'sublets',	'referrer_status',	'active',	'ajax_statsd',	'popular',	'social_connections',	'reviews',	'department',	'search',	'google_importer',	'update_reservation_requirements',	'collections',	'tos_confirm',	'airbnb_picks',	'update',	'new',	'signup_weibo_referral',	'zendesk_login_jwt',	'life',	'recommendations',	'ajax_payout_options_by_country',	'domains',	'create_paypal',	'office_location',	'popular_listing',	'payout_update',	'host_2013',	'ajax_payout_edit',	'why_host',	'apply_code',	'currencies',	'maybe_information',	'overview',	'how_it_works',	'country_options',	'host_summary',	'uptodate',	'position',	'create_ach',	'unread',	'departments',	'apply',	'weibo_signup_referral_finish',	'manage_listing',	'ajax_worth',	'hospitality',	'phone_verification_modal',	'payoneer_account_redirect',	'set_user',	'home_safety_terms',	'united-states',	'new_host',	'check',	'media_resources',	'pricing',	'recommend',	'has_profile_pic',	'place_worth',	'payoneer_signup_complete',	'badge',	'invalid_action',	'show_code',	'reputation',	'widget',	'change_availability')

action_list <- c(action_list,action_list_1)

cat("format_session.R - Create new action rows for train data\n")
train_actions_df <- as.data.frame(matrix(0,nrow=nrow(train_data),ncol = length(action_list)))
names(train_actions_df) <- paste("action_",action_list,sep="")
train_actions_df$id <- train_data$id

cat("format_session.R - Reduce session size before subsetting.")
sessions_train <- sqldf('select user_id, action, count(*) as count from sessions_train group by user_id,action')
sessions_train <- sessions_train[sessions_train$action %in% action_list,]
cat("format_session.R - Populate session rows for train - Counter goes to 73K\n")
t <- Sys.time()
for(i in 1:length(train_id)){
  tsa <- sessions_train$action[sessions_train$user_id==train_id[i]]
  if(length(tsa)>0){
    train_actions_df[train_actions_df$id==train_id[i],which(action_list %in% tsa)] <- 1
  }
  if(i%%1000==0) {print(i)}
}
print(Sys.time()-t)
gc(reset=TRUE)

cat("format_session.R - Create new rows for test data\n")
test_actions_df <- as.data.frame(matrix(0,nrow=nrow(test_data),ncol = length(action_list)))
names(test_actions_df) <- paste("action_",action_list,sep="")
test_actions_df$id <- test_data$id

cat("Reduce session size before subsetting\n")
sessions_test <- sqldf('select user_id, action, count(*) as count from sessions_test group by user_id,action')
sessions_test <- sessions_test[sessions_test$action %in% action_list,]
cat("format_session.R - Populate session rows for test - Counter goes to 62K\n")
t<- Sys.time()
for(i in 1:length(test_id)){
  tsa <- sessions_test$action[sessions_test$user_id==test_id[i]]
  if(length(tsa)>0){
    test_actions_df[test_actions_df$id==test_id[i],which(action_list %in% tsa)] <- 1
  }
  if(i%%1000==0) {print(i)}
}
print(Sys.time()-t)
gc(reset=TRUE)

train_actions_df <- train_actions_df[,-match(c("id"),colnames(train_actions_df))]
train_data <- cbind(train_data,train_actions_df)

test_actions_df <- test_actions_df[,-match(c("id"),colnames(test_actions_df))]
test_data <- cbind(test_data,test_actions_df)

train_data <- train_data[,-match(c("session_available"),colnames(train_data))]
test_data <- test_data[,-match(c("session_available"),colnames(test_data))]

rm(test_actions_df,train_actions_df,sessions_test,sessions_train)
gc(reset=TRUE)

train_data <- train_data[,-match(c("tsfa_t","date_account_created","timestamp_first_active"),colnames(train_data))]
test_data <- test_data[,-match(c("tsfa_t","date_account_created","timestamp_first_active"),colnames(test_data))]

cat("format_session.R - Write output into files. These files are read by xgb models\n")
write.csv(train_data,"train_formatted.csv",row.names=FALSE)
write.csv(test_data,"test_formatted.csv",row.names=FALSE)