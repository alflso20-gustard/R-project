#install.packages("httr2")
#install.packages("jsonlite")
#install.packages("dplyr")

library(httr2)
library(jsonlite)
library(dplyr)

setwd("C:/R/bigdata")
getwd()
year <- 2013:2020
url <- "https://apis.data.go.kr/B490007/qualAcquPtcond/getQualAcquPtcond"
service_key <- "784f7548f00b088982d33f6dfa017e1df1a3cc1fe65503b4c547fab2e4a6a03d"

data_year <- function(year) {
  page <- 1
  all_page_list <- list()
  
  message(paste0(">>> ", year, "년 데이터 수집 시작..."))
  
  repeat {
    message(paste0("   [", year, "년] ", page, "페이지 요청 중..."))
    
    req <- request(url) %>%
      req_timeout(10) %>% # 10초 이상 응답 없으면 강제 종료 후 에러 처리
      req_url_query(
        serviceKey = service_key,
        numOfRows = "50",
        pageNo = as.character(page),
        dataFormat = "JSON",
        acquYy = as.character(year),
        qualgbCd = "T",
        jmCd = "1320"
      )
    
    res <- tryCatch({
      resp <- req_perform(req)
      resp_body_json(resp)
    }, error = function(e) {
      message(paste0("    ", page, "페이지 요청 에러: ", e$message))
      return("ERROR")
    })
    
    if (identical(res, "ERROR")) {
      message("네트워크 or 서버 문제로 중단합니다.")
      break
    }
    
    # 3. 응답 구조 확인 및 items 추출
    items <- res$body$items
    
    # 가져온 데이터가 없거나 빈 리스트이면 종료
    if (is.null(items) || length(items) == 0) {
      message("더 이상 가져올 데이터가 없습니다.")
      break
    }
    
    # 4. 데이터프레임 변환
    df_page <- tryCatch({
      items %>% bind_rows()
    }, error = function(e) {
      message("데이터 변환 실패")
      return(NULL)
    })
    
    if (is.null(df_page) || nrow(df_page) == 0) {
      break
    }
    
    all_page_list[[page]] <- df_page
    message(paste0("   -> ", page, "페이지 수집 성공 (", nrow(df_page), "건)"))
    
    page <- page + 1
    Sys.sleep(0.1) # 서버 보호용 0.1초 대기
  }
  
  if (length(all_page_list) == 0) {
    message(paste0(">>> ", year, "년 수집 결과 없음."))
    return(NULL)
  }
  
  final_df <- bind_rows(all_page_list)
  message(paste0(year, "년 총 ", nrow(final_df), "건 수집 완료!\n"))
  return(final_df)
}

years <- 2013:2020

# 2013~2020년 전체 수집
result_list <- lapply(years, data_year)
ds <- bind_rows(result_list)

# 결과 확인
head(ds)
nrow(ds)
class(ds)

# acquCnt 수치형 변환 및 YYYY-MM-01 형태의 날짜 컬럼 생성
ds_date <- ds %>%
  mutate(
    acquCnt = as.numeric(acquCnt),
    date = as.Date(paste(acquYy, sprintf("%02d", as.numeric(acquMm)), "01", sep = "-"))
  )

library(ggplot2)
library(extrafont)

font_import(pattern = "malgun", prompt = FALSE) # 맑은만 불러오기
loadfonts(device = "win")

# 성별 막대그래프

df <- ds %>%
  mutate(acquCnt = as.numeric(acquCnt))
  ggplot(df, aes(x = ageGrupNm, y = acquCnt, fill = genderNm)) +
  geom_col(position = "dodge") +
  theme_minimal(base_family = "Malgun Gothic") +
  labs(title = "정보처리기사 연령대 및 성별 취득 현황",
       x = "연령대", y = "취득 수", fill = "성별")

# 연월별 취득 선그래프
ds_date %>%
  mutate(date = as.Date(paste(acquYy, sprintf("%02d", as.numeric(acquMm)), "01", sep = "-"))) %>%
  group_by(date) %>%
  summarise(total_cnt = sum(acquCnt, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = date, y = total_cnt)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_text(
    aes(label = scales::comma(total_cnt)), # 표시할 숫자 (천 단위 콤마 포함)
    vjust = -0.5,                            # 점 위로 글자 이동
    hjust = -0.15,
    size = 3.5,                            # 글자 크기
    color = "#2b5c8f"                      # 글자 색상
  ) +
  # 점 위에 라벨이 제대로 보이도록 Y축 위쪽 여백 살짝 확보
  scale_y_continuous(
  labels = scales::comma, 
  expand = expansion(mult = c(0.05, 0.15))
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_family = "Malgun Gothic") +
  labs(title = "년별 정보처리기사 취득 추이",
       x = "취득 연월", y = "취득 수")

#install.packages('colorspace')
library(colorspace)
hcl_palettes(plot = TRUE)



df <- ds_date %>%
  group_by(rgnNm) %>%
  summarise(total_cnt = sum(acquCnt, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    label_text = paste0(rgnNm, "\n", format(total_cnt, big.mark = ","), "명")
  )

library(treemap)

treemap(
  df,
  index = "label_text",             
  vSize = "total_cnt",              
  vColor = "total_cnt",            
  type = "value",                   
  palette = "Blues",               
  title = "지역별 정보처리기사 취득자 수 분포",
  title.legend = "취득자 수",
  fontsize.labels = 12,     
  fontfamily.labels = "Malgun Gothic",
)


df_year <- ds %>%
  mutate(acquCnt = as.numeric(acquCnt)) %>%
  group_by(acquYy) %>%
  summarise(total_cnt = sum(acquCnt, na.rm = TRUE), .groups = "drop")

ggplot(df_year, aes(x = factor(acquYy), y = total_cnt)) +
  geom_col(fill = "#2b5c8f", width = 0.6) + 
  # 막대 상단에 숫자 라벨 표시
  geom_text(
    aes(label = paste0(format(total_cnt, big.mark = ","), "명")),
    vjust = -0.5,                          
    size = 4,
    fontface = "bold",
    color = "#2b5c8f"
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  theme_minimal(base_family = "Malgun Gothic") +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    panel.grid.major.x = element_blank(),     # 세로 격자선 제거로 깔끔하게 정리
    axis.text = element_text(size = 11)
  ) +
  labs(
    title = "연도별 정보처리기사 취득자 수 추이",
    x = "취득 연도",
    y = "총 취득자 수"
  )

library(tidytext) # tidytext::reorder_within 필요


# 연도 x 지역별 하위 5개 데이터 추출
df_bottom <- ds_clean %>%
  group_by(acquYy, rgnNm) %>%
  summarise(total_cnt = sum(acquCnt, na.rm = TRUE), .groups = "drop_last") %>%
  # 각 연도별로 total_cnt가 가장 적은 하위 5개 선택
  slice_min(order_by = total_cnt, n = 10) %>%
  ungroup()

# 연도별로 막대 순서 정렬
df_bottom %>%
  mutate(rgnNm = reorder_within(rgnNm, total_cnt, acquYy)) %>%
  ggplot(aes(x = rgnNm, y = total_cnt, fill = factor(acquYy))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~acquYy, scales = "free_y") +
  scale_x_reordered() + # 연도별 개별 순서 적용
  coord_flip() + # 가로 막대로 전환
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_family = "Malgun Gothic") +
  labs(
    title = "연도별 정보처리기사 취득자 수 하위 10개 지역",
    x = "지역",
    y = "취득자 수"
  )
