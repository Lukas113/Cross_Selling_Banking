get_all_clients <- function() {
  check_connection()
  data = dbGetQuery(con, "SELECT *,
	CASE WHEN MOD(birth_number / 100, 100) > 50 THEN 
		'f'
	ELSE
		'm'
	END as gender,
	CASE WHEN MOD(birth_number / 100, 100) > 50 THEN
		TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD')
	ELSE
		TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD')
	END as birthdate,
	EXTRACT(YEAR FROM AGE('1998-12-31', CASE WHEN MOD(birth_number / 100, 100) > 50 THEN TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD') ELSE TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD') END)) as age
  FROM client")
  return(data)
}

get_count_by_gender <- function() {
  check_connection()
  data = dbGetQuery(con, "SELECT CASE WHEN MOD(birth_number / 100, 100) > 50 THEN 'Weiblich' ELSE 'Männlich' END as gender, count(CASE WHEN MOD(birth_number / 100, 100) > 50 THEN 'f' ELSE 'm' END) FROM client group by gender")
  return(data)
}

get_card_count_by_card_type <- function() {
  check_connection()
  data = dbGetQuery(con, "SELECT type, count(type) FROM card group by type")
  data$type <- factor(data$type, levels = c("classic", "junior", "gold"))
  return(data)
}

get_distinct_accounts_by_card <- function () {
  check_connection()
  data <- dbGetQuery(con, "select 'Accounts' as accounts, count(distinct(disp.account_id)) from card join disp on card.disp_id = disp.disp_id")
  return(data)
}

get_card_count_by_disp_type <- function() {
  check_connection()
  data <- dbGetQuery(con, "select distinct(disp.type), count(disp.type) from card join disp on card.disp_id = disp.disp_id group by disp.type")
  return(data)
}

get_client_age_by_issued_date <- function() {
  check_connection()
  data = dbGetQuery(con, "select card.card_id, card.type, card.issued, client.*,
CASE WHEN MOD(client.birth_number / 100, 100) > 50 THEN
  	TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD')
  ELSE
  	TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD')
  END as birthdate,
  EXTRACT(YEAR FROM AGE('1998-12-31', CASE WHEN MOD(birth_number / 100, 100) > 50 THEN TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD') ELSE TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD') END)) as current_age,
  EXTRACT(YEAR FROM AGE(card.issued, CASE WHEN MOD(birth_number / 100, 100) > 50 THEN TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD') ELSE TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD') END)) as age_on_card_issue
from card
join disp on card.disp_id = disp.disp_id
join client on disp.client_id = client.client_id")
  return(data)
}

get_clients_without_credit_card_by_age_range <- function(from_age = 0, to_age = 25) {
  check_connection()
  data = dbGetQuery(con, sprintf("select disp.disp_id, disp.account_id, disp.client_id,
  EXTRACT(YEAR FROM AGE('1998-12-31', CASE WHEN MOD(birth_number / 100, 100) > 50 THEN TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD') ELSE TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD') END)) as age
  from account
  
  join disp on account.account_id = disp.account_id
  join client on disp.client_id = client.client_id
  
  where not exists(
    select * from disp where account.account_id = disp.account_id and exists (
      	select * from card where disp.disp_id = card.disp_id
      )
    )
    
  and disp.type = 'OWNER'
  and EXTRACT(YEAR FROM AGE('1998-12-31', CASE WHEN MOD(birth_number / 100, 100) > 50 THEN TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD') ELSE TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD') END)) > %s
  and EXTRACT(YEAR FROM AGE('1998-12-31', CASE WHEN MOD(birth_number / 100, 100) > 50 THEN TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD') ELSE TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD') END)) < %s
  order by account.account_id", from_age, to_age))
  return(data)
}