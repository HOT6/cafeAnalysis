
##### 생활 인구 ####
seoul_pop <- read.csv("data/seoul_population.csv")
seoul_detail_pop <- read.csv("data/LOCAL_PEOPLE_20201107.csv", stringsAsFactors = F)

seoul_pop <- seoul_pop %>% 
  dplyr::select(-남자0세부터9세생활인구수, -여자0세부터9세생활인구수)

seoul_detail_pop <- seoul_detail_pop %>% 
  dplyr::select(-남자0세부터9세생활인구수, -여자0세부터9세생활인구수)


seoul_pop <- rename(seoul_pop,
                    hour = "시간대구분", 
                    adm_cd2 = "행정동코드",
                    total_pop = "총생활인구수",
                    m10_14 = "남자10세부터14세생활인구수",
                    m15_19 = "남자15세부터19세생활인구수",
                    m20_24 = "남자20세부터24세생활인구수",
                    m25_29 = "남자25세부터29세생활인구수",
                    m30_34 = "남자30세부터34세생활인구수",
                    m35_39 = "남자35세부터39세생활인구수",
                    m40_44 = "남자40세부터44세생활인구수",
                    m45_49 = "남자45세부터49세생활인구수",
                    m50_54 = "남자50세부터54세생활인구수",
                    m55_59 = "남자55세부터59세생활인구수",
                    m60_64 = "남자60세부터64세생활인구수",
                    m65_69 = "남자65세부터69세생활인구수",
                    m70 = "남자70세이상생활인구수",
                    f10_14 = "여자10세부터14세생활인구수",
                    f15_19 = "여자15세부터19세생활인구수",
                    f20_24 = "여자20세부터24세생활인구수",
                    f25_29 = "여자25세부터29세생활인구수",
                    f30_34 = "여자30세부터34세생활인구수",
                    f35_39 = "여자35세부터39세생활인구수",
                    f40_44 = "여자40세부터44세생활인구수",
                    f45_49 = "여자45세부터49세생활인구수",
                    f50_54 = "여자50세부터54세생활인구수",
                    f55_59 = "여자55세부터59세생활인구수",
                    f60_64 = "여자60세부터64세생활인구수",
                    f65_69 = "여자65세부터69세생활인구수",
                    f70 = "여자70세이상생활인구수",
)

seoul_detail_pop <- rename(seoul_detail_pop,
                    hour = "시간대구분", 
                    TOT_REG_CD = "집계구코드",
                    adm_cd2 = "행정동코드",
                    total_pop = "총생활인구수",
                    m10_14 = "남자10세부터14세생활인구수",
                    m15_19 = "남자15세부터19세생활인구수",
                    m20_24 = "남자20세부터24세생활인구수",
                    m25_29 = "남자25세부터29세생활인구수",
                    m30_34 = "남자30세부터34세생활인구수",
                    m35_39 = "남자35세부터39세생활인구수",
                    m40_44 = "남자40세부터44세생활인구수",
                    m45_49 = "남자45세부터49세생활인구수",
                    m50_54 = "남자50세부터54세생활인구수",
                    m55_59 = "남자55세부터59세생활인구수",
                    m60_64 = "남자60세부터64세생활인구수",
                    m65_69 = "남자65세부터69세생활인구수",
                    m70 = "남자70세이상생활인구수",
                    f10_14 = "여자10세부터14세생활인구수",
                    f15_19 = "여자15세부터19세생활인구수",
                    f20_24 = "여자20세부터24세생활인구수",
                    f25_29 = "여자25세부터29세생활인구수",
                    f30_34 = "여자30세부터34세생활인구수",
                    f35_39 = "여자35세부터39세생활인구수",
                    f40_44 = "여자40세부터44세생활인구수",
                    f45_49 = "여자45세부터49세생활인구수",
                    f50_54 = "여자50세부터54세생활인구수",
                    f55_59 = "여자55세부터59세생활인구수",
                    f60_64 = "여자60세부터64세생활인구수",
                    f65_69 = "여자65세부터69세생활인구수",
                    f70 = "여자70세이상생활인구수",
)

seoul_pop <- seoul_pop %>% 
  group_by(adm_cd2, hour) %>% 
  summarise(mean_pop = mean(total_pop),
            m10 = (sum(m10_14) + sum(m15_19)) / 2,
            m20 = (sum(m20_24) + sum(m25_29)) / 2,
            m30 = (sum(m30_34) + sum(m35_39)) / 2,
            m40 = (sum(m40_44) + sum(m45_49)) / 2,
            m50 = (sum(m50_54) + sum(m55_59) + sum(m60_64) + sum(m65_69) + sum(m70)) / 5,
            f10 = (sum(f10_14) + sum(f15_19)) / 2,
            f20 = (sum(f20_24) + sum(f25_29)) / 2,
            f30 = (sum(f30_34) + sum(f35_39)) / 2,
            f40 = (sum(f40_44) + sum(f45_49)) / 2,
            f50 = (sum(f50_54) + sum(f55_59) + sum(f60_64) + sum(f65_69) + sum(f70)) / 5
  )



seoul_detail_pop[seoul_detail_pop$m10_14 == "*", "m10_14"] <-'0'
seoul_detail_pop$m10_14 <- as.numeric(seoul_detail_pop$m10_14)

seoul_detail_pop[seoul_detail_pop$m15_19 == "*", "m15_19"] <- '0'
seoul_detail_pop$m15_19 <- as.numeric(seoul_detail_pop$m15_19)

seoul_detail_pop[seoul_detail_pop$m20_24 == "*", "m20_24"] <- '0'
seoul_detail_pop$m20_24 <- as.numeric(seoul_detail_pop$m20_24)

seoul_detail_pop[seoul_detail_pop$m25_29 == "*", "m25_29"] <- '0'
seoul_detail_pop$m25_29 <- as.numeric(seoul_detail_pop$m25_29)

seoul_detail_pop[seoul_detail_pop$m30_34 == "*", "m30_34"] <- '0'
seoul_detail_pop$m30_34 <- as.numeric(seoul_detail_pop$m30_34)

seoul_detail_pop[seoul_detail_pop$m35_39 == "*", "m35_39"] <- '0'
seoul_detail_pop$m35_39 <- as.numeric(seoul_detail_pop$m35_39)

seoul_detail_pop[seoul_detail_pop$m40_44 == "*", "m40_44"] <- '0'
seoul_detail_pop$m40_44 <- as.numeric(seoul_detail_pop$m40_44)

seoul_detail_pop[seoul_detail_pop$m45_49 == "*", "m45_49"] <- '0'
seoul_detail_pop$m45_49 <- as.numeric(seoul_detail_pop$m45_49)

seoul_detail_pop[seoul_detail_pop$m50_54 == "*", "m50_54"] <- '0'
seoul_detail_pop$m50_54 <- as.numeric(seoul_detail_pop$m50_54)

seoul_detail_pop[seoul_detail_pop$m55_59 == "*", "m55_59"] <- '0'
seoul_detail_pop$m55_59 <- as.numeric(seoul_detail_pop$m55_59)

seoul_detail_pop[seoul_detail_pop$m60_64 == "*", "m60_64"] <- '0'
seoul_detail_pop$m60_64 <- as.numeric(seoul_detail_pop$m60_64)

seoul_detail_pop[seoul_detail_pop$m65_69 == "*", "m65_69"] <- '0'
seoul_detail_pop$m65_69 <- as.numeric(seoul_detail_pop$m65_69)

seoul_detail_pop[seoul_detail_pop$m70 == "*", "m70"] <- '0'
seoul_detail_pop$m70 <- as.numeric(seoul_detail_pop$m70)


seoul_detail_pop[seoul_detail_pop$f10_14 == "*", "f10_14"] <- '0'
seoul_detail_pop$f10_14 <- as.numeric(seoul_detail_pop$f10_14)

seoul_detail_pop[seoul_detail_pop$f15_19 == "*", "f15_19"] <- '0'
seoul_detail_pop$f15_19 <- as.numeric(seoul_detail_pop$f15_19)

seoul_detail_pop[seoul_detail_pop$f20_24 == "*", "f20_24"] <- '0'
seoul_detail_pop$f20_24 <- as.numeric(seoul_detail_pop$f20_24)

seoul_detail_pop[seoul_detail_pop$f25_29 == "*", "f25_29"] <- '0'
seoul_detail_pop$f25_29 <- as.numeric(seoul_detail_pop$f25_29)

seoul_detail_pop[seoul_detail_pop$f30_34 == "*", "f30_34"] <- '0'
seoul_detail_pop$f30_34 <- as.numeric(seoul_detail_pop$f30_34)

seoul_detail_pop[seoul_detail_pop$f35_39 == "*", "f35_39"] <- '0'
seoul_detail_pop$f35_39 <- as.numeric(seoul_detail_pop$f35_39)

seoul_detail_pop[seoul_detail_pop$f40_44 == "*", "f40_44"] <- '0'
seoul_detail_pop$f40_44 <- as.numeric(seoul_detail_pop$f40_44)

seoul_detail_pop[seoul_detail_pop$f45_49 == "*", "f45_49"] <- '0'
seoul_detail_pop$f45_49 <- as.numeric(seoul_detail_pop$f45_49)

seoul_detail_pop[seoul_detail_pop$f50_54 == "*", "f50_54"] <- '0'
seoul_detail_pop$f50_54 <- as.numeric(seoul_detail_pop$f50_54)

seoul_detail_pop[seoul_detail_pop$f55_59 == "*", "f55_59"] <- '0'
seoul_detail_pop$f55_59 <- as.numeric(seoul_detail_pop$f55_59)

seoul_detail_pop[seoul_detail_pop$f60_64 == "*", "f60_64"] <- '0'
seoul_detail_pop$f60_64 <- as.numeric(seoul_detail_pop$f60_64)

seoul_detail_pop[seoul_detail_pop$f65_69 == "*", "f65_69"] <- '0'
seoul_detail_pop$f65_69 <- as.numeric(seoul_detail_pop$f65_69)

seoul_detail_pop[seoul_detail_pop$f70 == "*", "f70"] <- '0'
seoul_detail_pop$f70 <- as.numeric(seoul_detail_pop$f70)



seoul_detail_pop <- seoul_detail_pop %>% 
  group_by(TOT_REG_CD, hour) %>% 
  summarise(mean_pop = mean(total_pop),
            m10 = (sum(m10_14) + sum(m15_19)) / 2,
            m20 = (sum(m20_24) + sum(m25_29)) / 2,
            m30 = (sum(m30_34) + sum(m35_39)) / 2,
            m40 = (sum(m40_44) + sum(m45_49)) / 2,
            m50 = (sum(m50_54) + sum(m55_59) + sum(m60_64) + sum(m65_69) + sum(m70)) / 5,
            f10 = (sum(f10_14) + sum(f15_19)) / 2,
            f20 = (sum(f20_24) + sum(f25_29)) / 2,
            f30 = (sum(f30_34) + sum(f35_39)) / 2,
            f40 = (sum(f40_44) + sum(f45_49)) / 2,
            f50 = (sum(f50_54) + sum(f55_59) + sum(f60_64) + sum(f65_69) + sum(f70)) / 5
  )




seoul_pop$adm_cd2 <- paste0(seoul_pop2$adm_cd2, '00')

write.csv(seoul_detail_pop, "data/seoul_detail_pop.csv", row.names = T)
write.csv(seoul_pop2, "data/seoul_pop.csv", row.names = T)


##### 서울 상권 정보 ####
seoul_commercial <- read_excel("data/seoul_commercial.xlsx")

seoul_commercial <- seoul_commercial %>% 
  dplyr::select(상호명, 지점명, 상권업종대분류명, 상권업종중분류명, 상권업종소분류명, 행정동명, 도로명주소, 경도, 위도, 행정동코드)
seoul_commercial <- seoul_commercial %>% filter(!is.na(seoul_commercial$위도))

# 커피점 필터링
seoul_cafe <- seoul_commercial %>% filter(상권업종중분류명 == "커피점/카페")


colnames(seoul_cafe)[10]  <- "adm_cd2"

# 수유1동
seoul_cafe[seoul_cafe$adm_cd2 == 1130561000, ]$adm_cd2 <- 1130561500
# 수유2동
seoul_cafe[seoul_cafe$adm_cd2 == 1130562000, ]$adm_cd2 <- 1130562500
# 번1동
seoul_cafe[seoul_cafe$adm_cd2 == 1130559000, ]$adm_cd2 <- 1130559500
# 번2동
seoul_cafe[seoul_cafe$adm_cd2 == 1130560000, ]$adm_cd2 <- 1130560300


write.csv(seoul_cafe, 'data/seoul_cafe.csv', row.names = T)




##### 집계구 #### 
library(rgdal)
seoul <-
  readOGR(
    dsn = 'data/seoul3',
    layer = 'seoul3',
    encoding = 'utf-8')


seoul_deatil <-
  readOGR(
    dsn = 'data/seoul_detail',
    layer = 'seoul_detail',
    encoding = 'utf-8')


seoul_deatil@polygons[[1]]@Polygons[[1]]@coords %>% head(n = 10L)

# GRS80 좌표계를 WGS84 좌표계로 변환.
seoul_deatil <- spTransform(x = seoul_deatil, CRSobj = CRS('+proj=longlat +datum=WGS84'))

seoul_deatil@polygons[[1]]@Polygons[[1]]@coords %>% head(n = 10L)

# save seoul_deatil as shapefiles
writeOGR(
  obj = seoul_deatil,
  dsn = 'data/simple',
  layer = 'seoul_detail',
  driver = 'ESRI Shapefile')



seoul <-
  readOGR(
    dsn = 'data/simple',
    layer = 'seoul_detail',
    encoding = 'utf-8')
