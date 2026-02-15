## FORTRAN

### **I/O**
1. 輸出：print 與 write 📢
我們最常用的是 print *，這裡的星號 * 代表「使用預設格式」。
    - print 語法：
        ```fortran
        print *, "這是一個字串", 123, 45.6
        ```
    - write 語法：
        比 print 更強大，可以指定輸出的位置（如檔案）。
        ```fortran
        write(*,*) "這是用 write 印出的內容"
        ! 第一個星號代表螢幕，第二個代表預設格式
        ```

2. 輸入：read 📥
當你需要使用者從鍵盤輸入資料時，就會用到 read。
    - read 語法：
        ```fortran
        integer :: age
        print *, "請輸入你的年齡："
        read *, age
        print *, "原來你 ", age, " 歲啊！"
        ```

💡 小練習：整合應用
假設我們要寫一個小程式，請使用者輸入圓形的半徑，然後計算並印出面積（面積公式為 A = \pi r^2）。
我們已經宣告了：
real, parameter :: pi = 3.14159
real :: radius, area

現在請你試著寫出：
 * 讀取使用者輸入的半徑 (radius)。
 * 計算面積並存入 area。
 * 最後印出結果。
你會怎麼寫這三行程式碼呢？（提示：Fortran 的次方運算子是 **，例如 r^2 寫作 radius**2）

### **型別**

#### `integer` 整數
##### 迴圈計數器

#### `real` 有小數點的實數

#### `complex` 複數
##### 處理幾何

#### `logical` 布林值
`.true.` 或 `.false.`

#### `character` 字元或字串
加上 `(len=N)` 來指定長度


### **屬性**

#### `parameter` (保護常數)
```
real, parameter :: pi = 3.14159265
real, parameter :: c = 299792458.0
```
如果程式後面嘗試修改？編譯器會報錯攔截

#### `dimension` (處理陣列)
#### `allocatable` (動態彈性)

### **控制**

#### `if`
1. 單行 if (Logical IF)
如果你只需要在條件成立時執行一件小事，可以用最簡單的一行寫法：
if (temperature > 100.0) print *, "警告：水分已沸騰！"

這種寫法不需要 then 也不需要 end if，適合處理簡單的錯誤檢查。
2. 標準塊狀 if (IF-THEN-END IF) 🧱
這是最常用的結構，適合當條件成立時需要執行多行指令：
if (velocity < 0.0) then
    velocity = abs(velocity)  ! 取絕對值
    direction = -1            ! 標記方向反向
end if

3. 多重分支 if (IF-ELSE IF-ELSE) 🚦
當你有三種以上的狀況要處理時，就會用到 else if：
if (x > 0.0) then
    print *, "正數"
else if (x < 0.0) then
    print *, "負數"
else
    print *, "這是零"
end if

⚠️ 重要細節：邏輯運算子
在寫條件（括號裡的部分）時，你會用到比較符號。現代 Fortran 支援兩種寫法，你可以挑順眼的用：
| 數學意義 | 現代符號 | 老派寫法 (仍通用) |
|---|---|---|
| 大於 / 小於 | >, < | .gt. / .lt. |
| 大於等於 / 小於等於 | >=, <= | .ge. / .le. |
| 等於 / 不等於 | ==, /= | .eq. / .ne. |



#### `do loop`
#### `select case`

### **結構**

#### `block`











