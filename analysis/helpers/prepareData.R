library("RPostgreSQL")

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
  FROM client LIMIT 10")
  return(data)
}