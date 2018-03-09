
library(readxl)
library(data.table)
library(stringr)
data <- read_excel("E:/tmp_data.xlsx")
setDT(data) # data <- as.data.table(data) data.frame转换为data.table格式，这两句是等价的
data[, c("Left", "Right") := tstrsplit(Content, "+", fixed = TRUE)]
# 通过“+”把原字符串分成两列，分别命名为Left和Right
data_left <- data[, .(Left)]
data_right<- data[, .(Right)]
# 如果数据格式都是一个字母一个长度为4的数字然后括号里一个一位小数这么整齐的话可以这么做：
data_left[, `:=`(
  Left_1 = str_sub(Left, 1, 1),
  Left_2 = str_sub(Left, 2, 5),
  Left_3 = str_sub(Left, 7, 9))
  ]
# 在data.table里 :=表示对列进行赋值，上面这段表示从Left这列里分别截取固定位置的字符赋值给Left_1 Left_2 Left_3三列
# 进行下一步处理的时候要注意4000还是字符串格式，如果后续要进行运算的话需要转换为numeric

#如果数据格式不一定是那么整齐的话就需要用正则表达式来处理：
data_right[, `:=`(
  Right_1 = str_extract(Right, "^[A-Z]+"),
  Right_2 = str_extract(Right, "\\d+"),
  Right_3 = str_extract(str_extract(Right, "\\(.+\\)$"), "\\d.+\\d")
)]
# 这个正则表达式^[A-Z]表示以大写字母A-Z打头，+表示后面持续有几个大写字母就都截出来；
# \\d+表示截取第一个出现的数字出来，这里我感觉写的不够严谨，但是能用就先这样吧；
# 第三步是先把括号后面的内容截取出来，然后再从里面把数字摘取出来。这个分两步写的感觉很冗余，但是暂时不会改就先这样吧。。