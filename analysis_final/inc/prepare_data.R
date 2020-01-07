get_accounts_running_total <- function() {
  check_connection()
  data = dbGetQuery(con, "SELECT date, SUM(SUM(1)) OVER(ORDER BY date) AS accounts_running_total FROM account GROUP BY date")
  return(data)
}

get_all_clients_age <- function() {
  check_connection()
  data = dbGetQuery(con, "SELECT EXTRACT(YEAR FROM AGE('1998-12-31', CASE WHEN MOD(birth_number / 100, 100) > 50 THEN TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD') ELSE TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD') END)) as age FROM client")
  return(data)
}

get_count_by_gender <- function() {
  check_connection()
  data = dbGetQuery(con, "SELECT CASE WHEN MOD(birth_number / 100, 100) > 50 THEN 'Weiblich' ELSE 'Männlich' END as gender, count(CASE WHEN MOD(birth_number / 100, 100) > 50 THEN 'f' ELSE 'm' END) FROM client group by gender")
  return(data)
}

get_client_age_and_balance <- function() {
  check_connection()
  data = dbGetQuery(con, "SELECT AVG(trans.balance) as avg_balance, EXTRACT(YEAR FROM AGE('1998-12-31', CASE WHEN MOD(client.birth_number / 100, 100) > 50 THEN TO_DATE(CONCAT('19', CAST(client.birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD') ELSE TO_DATE(CONCAT('19', CAST(client.birth_number AS VARCHAR(6))), 'YYYYMMDD') END)) as age
FROM trans
join disp on trans.account_id = disp.account_id
join client on disp.client_id = client.client_id
WHERE
trans_id IN (SELECT MAX(trans_id) AS last_id FROM trans GROUP BY account_id) AND
disp.type = 'OWNER'
group by age")
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

get_owner_disp <- function() {
  check_connection()
  data <- dbGetQuery(con, "SELECT account_id, count(type) as count_users from disp group by account_id")
  return(data)
}

get_account_loans <- function(){
  check_connection()
  data <- dbGetQuery(con, "select account.account_id, count(loan.loan_id)
  from account
  Left join loan on account.account_id = loan.account_id
  group by account.account_id")
  return(data)
}

get_all_clients_join_region_ctype <- function() {
  check_connection()
  data <- dbGetQuery(con,"SELECT  client.client_id,
  CASE WHEN MOD(client.birth_number / 100, 100) > 50 THEN 
  'f'
  ELSE
  'm'
  END as gender,
  CASE WHEN MOD(client.birth_number / 100, 100) > 50 THEN
  TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD')
  ELSE
  TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD')
  END as birthdate,
  EXTRACT(YEAR FROM AGE('1998-12-31', CASE WHEN MOD(client.birth_number / 100, 100) > 50 THEN TO_DATE(CONCAT('19', CAST(client.birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD') ELSE TO_DATE(CONCAT('19', CAST(client.birth_number AS VARCHAR(6))), 'YYYYMMDD') END)) as age,
  card.type, district.a3 AS region
  FROM client
  JOIN disp ON client.client_id = disp.client_id
  LEFT JOIN card ON disp.disp_id = card.disp_id
  JOIN district ON client.district_id = district.a1
  ORDER BY client_id") 
  return(data)
}

get_accounts_with_loans <- function(){
  check_connection()
  data <- dbGetQuery(con, "SELECT account.account_id,
  account.date AS date_account_creation, loan.loan_id,loan.date , loan.amount AS loan_amount, loan.duration,
  loan.payments, 
  CASE 
	WHEN loan.status = 'A' THEN 'finished, OK'
	WHEN loan.status = 'B' THEN 'finished, unpayed'
	WHEN loan.status = 'C' THEN 'running, OK'
	WHEN loan.status = 'D' THEN 'running, in debt'
  END AS payment_status
  FROM account 
  LEFT JOIN loan ON account.account_id = loan.account_id
  GROUP BY account.account_id, loan.loan_id
  ORDER BY account.date")
  return(data)
}

get_loan_clients <- function(){
  check_connection()
  data <- dbGetQuery(con, "select loan.loan_id, loan.date as loan_date, loan.amount as loan_amount, 
  loan.duration as loan_duration, CASE 
	WHEN loan.status = 'A' THEN 'finished - no problems'
  	WHEN loan.status = 'B' THEN 'finished - unpayed'
  	WHEN loan.status = 'C' THEN 'running - OK'
  	WHEN loan.status = 'D' THEN 'running - in debt' 
	END AS loan_status,
  CASE WHEN MOD(birth_number / 100, 100) > 50 THEN 
		'F'
	ELSE
		'M'
	END as gender,
	EXTRACT(YEAR FROM AGE('1998-12-31', CASE WHEN MOD(birth_number / 100, 100) > 50 THEN TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD') ELSE TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD') END)) as age,
	account.account_id, card.card_id
  FROM disp
  full join card on disp.disp_id = card.disp_id
  join client on client.client_id = disp.client_id
  join account on disp.account_id	 = account.account_id
  join loan on account.account_id = loan.account_id
  where disp.type = 'OWNER'
  order by loan.loan_id")
  return(data)
}


get_accounts_services <- function(){
  check_connection()
  data <- dbGetQuery(con, "select account.account_id, account.date as account_issued ,card.issued as card_issued, loan.date as loan_date, min(trans.date) as order_date
  from account
  left join loan on account.account_id = loan.account_id
  left join disp on disp.account_id = account.account_id
  left join card on card.disp_id = disp.disp_id
  left join orders on account.account_id = orders.account_id
  left join trans on (orders.account_id = trans.account_id AND orders.amount = trans.amount AND orders.account_to = trans.account AND orders.bank_to = trans.bank)
  where disp.type = 'OWNER'
  group by account.account_id, account.date ,card.issued, loan.date")
  return(data)
}
