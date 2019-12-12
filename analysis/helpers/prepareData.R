library("RPostgreSQL")
DB_HOST='server2053.cs.technik.fhnw.ch' # or 86.119.36.94 depending on the network
DB_PORT = 5432
DB_DBNAME = 'bank_db' # or 'warenkorb_db'
DB_USERNAME = 'db_user' 
DB_PASSWORD = 'db_user_pw' 
# load the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# connect to the database
con <- dbConnect(drv, dbname = DB_DBNAME,
                 host = DB_HOST, port = DB_PORT,
                 user = DB_USERNAME, password = DB_PASSWORD)


get_card_count_by_disp_type <- function() {
  data <- dbGetQuery(con, "select distinct(disp.type), count(disp.type) from card join disp on card.disp_id = disp.disp_id group by disp.type")
  return(data)
}

get_all_card_count <- function() {
  data <- dbGetQuery(con, "select 'Cards' as cards, count(card_id) from card")
  return(data)
}

get_card_count_by_card_type <- function() {
  data = dbGetQuery(con, "SELECT type, count(type) FROM card group by type")
  data$type <- factor(data$type, levels = c("classic", "junior", "gold"))
  return(data)
}

get_distinct_accounts_by_card <- function () {
  data <- dbGetQuery(con, "select 'Accounts' as accounts, count(distinct(disp.account_id)) from card join disp on card.disp_id = disp.disp_id")
  return(data)
}

get_months_until_cards_issued <- function () {
  data <- dbGetQuery(con, "select card.type, card.issued, disp.account_id, account.date, 
	((DATE_PART('year', card.issued) - DATE_PART('year', account.date)) * 12) + (DATE_PART('month', card.issued) - DATE_PART('month', account.date)) as months_until_card
from card
join disp on card.disp_id = disp.disp_id
join account on disp.account_id = account.account_id")
  return(data)
}

get_count_by_gender <- function() {
  data = dbGetQuery(con, "SELECT CASE WHEN MOD(birth_number / 100, 100) > 50 THEN 'f' ELSE 'm' END as gender, count(CASE WHEN MOD(birth_number / 100, 100) > 50 THEN 'f' ELSE 'm' END) FROM client group by gender")
  return(data)
}

get_client_age_by_issued_date <- function() {
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

get_count_by_client_age <- function() {
  data = dbGetQuery(con, "SELECT
	EXTRACT(YEAR FROM AGE('1998-12-31', CASE WHEN MOD(birth_number / 100, 100) > 50 THEN TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD') ELSE TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD') END)) as age,
	count(EXTRACT(YEAR FROM AGE('1998-12-31', CASE WHEN MOD(birth_number / 100, 100) > 50 THEN TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD') ELSE TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD') END)))
  FROM client
  group by age")
  return(data)
}

get_districts <- function() {
  data <- dbGetQuery(con,"SELECT a1 as district_code, a2 as district_name,
  a3 as region, a4 as no_of_inhabitants, a5 as municipalities_u499,
  a6 as municipalties_500_1999, a7 as municipalties_2000_9999, a8 as municipalties_o9999,
  a9 as no_of_cities, a10 as ratio_urban_inhabitants, a11 as avg_salary,
  a12 as unemploymt_rate95, a13 as unemploymt_rate96, a14 as no_enterpreneurs_per_1000,
  a15 as commited_crimes95, a16 as commited_crimes96 
  from district")
  return(data)
}

get_all_clients_join_region_ctype <- function() {
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


get_all_clients <- function() {
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

get_check_services <- function(){
  data <- dbGetQuery(con,"SELECT disp.account_id, card.card_id, card.issued AS card_issued,
  loan.loan_id, loan.payments AS loan_payments,  orders.orders_id, trans.trans_id, trans.date AS transaction_date,
  trans.amount AS transaction_amount
  FROM disp
  FULL JOIN card ON disp.disp_id = card.disp_id
  FULL JOIN account ON disp.account_id = account.account_id
  FULL JOIN loan ON account.account_id = loan.account_id
  FULL JOIN orders ON account.account_id = orders.account_id
  LEFT JOIN trans ON account.account_id = trans.account_id
  WHERE DATE_PART('year', card.issued ) = 1998 OR card.card_id IS NULL
  ORDER BY account.account_id ")
  return (data)
}

get_check_services_all <- function(){
  data <- dbGetQuery(con, "SELECT account.account_id, card.card_id, disp.type AS account_type,
  loan.loan_id, loan.payments AS loan_payments,  orders.orders_id, 
 CASE WHEN MOD(birth_number / 100, 100) > 50 THEN
		TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD')
	ELSE
		TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD')
	END as birthdate,
	EXTRACT(YEAR FROM AGE('1998-12-31', CASE WHEN MOD(birth_number / 100, 100) > 50 THEN TO_DATE(CONCAT('19', CAST(birth_number-5000 AS VARCHAR(6))), 'YYYYMMDD') ELSE TO_DATE(CONCAT('19', CAST(birth_number AS VARCHAR(6))), 'YYYYMMDD') END)) as age
  FROM disp
  INNER JOIN client on disp.client_id = client.client_id
  FULL JOIN card ON disp.disp_id = card.disp_id
  FULL JOIN account ON disp.account_id = account.account_id
  FULL JOIN loan ON account.account_id = loan.account_id
  FULL JOIN orders ON account.account_id = orders.account_id
  LEFT JOIN trans ON account.account_id = trans.account_id
  WHERE DATE_PART('year', card.issued ) = 1998 OR card.card_id IS NULL
  ORDER BY account.account_id  " )
  return(data)
}

get_permanent_orders <- function(){
  data <- dbGetQuery(con, "SELECT trans.trans_id, trans.date, orders.orders_id, orders.account_id, orders.account_to, orders.amount,  
  CASE 
  WHEN orders.k_symbol = 'SIPO' THEN 'household' 
  WHEN orders.k_symbol = 'LEASING' THEN 'leasing'
  WHEN orders.k_symbol = 'UVER' THEN 'loan payment'
  WHEN orders.k_symbol = 'POJISTNE' THEN 'insurrance payment'
  WHEN orders.k_symbol = ' ' THEN 'NA'
  END
  FROM orders
  INNER JOIN trans ON orders.account_id = trans.account_id
  WHERE orders.amount = trans.amount 
  ORDER BY account_id, orders_id, date" )
  return(data)
}



get_disp_transactions <- function() {
  data <- dbGetQuery(con,"SELECT disp.disp_id, disp.client_id, disp.account_id, disp.type, trans.trans_id, trans.date,
  CASE
  WHEN trans.type = 'PRIJEM' THEN 'credit'
  WHEN trans.type = 'VYDAJ' THEN 	'withdrawal'
  END AS transaction_type,
  CASE 
  WHEN trans.operation = 'VYBER KARTOU' THEN 'credit card withdrawal'
  WHEN trans.operation = 'VKLAD' THEN 'credit in cash'
  WHEN trans.operation = 'PREVOD Z UCTU' THEN 'collection of other bank'
  WHEN trans.operation = 'VYBER' THEN 'withdrawal in cash'
  WHEN trans.operation = 'PREVOD NA UCET' THEN 'remittance to another bank' 
  END AS transaction_operation,
  trans.amount, ROUND(trans.balance) AS balance_after_trans
  FROM disp 
  LEFT JOIN trans ON disp.account_id = trans.account_id
  ORDER BY trans.trans_id")
  return(data)
}

get_loan_clients <- function(){
  data <- dbGetQuery(con, "select loan.loan_id, loan.amount as loan_amount, 
  loan.duration as loan_duration,loan.payments as loan_payments, CASE 
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
	account.account_id
  FROM disp
  join client on client.client_id = disp.client_id
  join account on disp.account_id	 = account.account_id
  join loan on account.account_id = loan.account_id
  where disp.type = 'OWNER'
  order by loan.loan_id")
  return(data)
}


get_owner_disp <- function(){
  data <- dbGetQuery(con, "SELECT account_id, count(type) as count_users from disp group by account_id")
  return(data)
}

get_account_loans <- function(){
  data <- dbGetQuery(con, "select account.account_id, count(loan.loan_id)
  from account
  Left join loan on account.account_id = loan.account_id
  group by account.account_id")
  return(data)
}

