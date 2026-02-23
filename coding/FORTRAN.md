<style>
  h1 { font-size: 1.80rem; }
  h2 { font-size: 1.65rem; }
  h3 { font-size: 1.50rem; }
  h4 { font-size: 1.35rem; }
  h5 { font-size: 1.20rem; }
  h6 { font-size: 1.05rem; }
  p { font-size: 1.00rem; }
</style>

# 🚀 Fortran 高效率速查筆記：目錄架構

## 1. 數據容器與記憶體

### 1-1. 基本型別速查 (Types)
在 Fortran 中，變數宣告的語法結構為：  
型別 (參數), 屬性1, 屬性2 :: 變數名稱 = 初始值  

| 型別 | 說明 | 範例 |
| :---- | :---- | :---- |
| **integer** | 整數 | integer :: count |
| **real** | 實數 (浮點數) | real :: distance |
| **complex** | 複數 | complex :: impedance |
| **logical** | 邏輯值 (.true. / .false.) | logical :: is_done |
| **character** | 字元或字串 | character(len=20) :: name |

#### 複數宣告與賦值 (Complex)
複數由「實部」與「虛部」組成。在 Fortran 中，定義複數的方式會根據你使用的是數值（常數）還是變數而有所不同：  
1. 使用常數賦值
    當實部與虛部都是固定的數值時，直接使用括號 (real, imag)：  
    ```fortran
    complex :: z1, z2, z_sum
    z1 = (3.0, 4.0)     ! 代表 3 + 4i
    z2 = (1.0, -2.0)    ! 代表 1 - 2i

    z_sum = z1 + z2     ! 結果為 (4.0, 2.0)
    ```
2. 使用變數賦值 (重要)
    如果你想用已經存在的變數 `x` 和 `y` 來組成複數，不能寫成 `(x, y)`，必須使用 `cmplx()` 函數：
    ```fortran
    real :: x = 3.0, y = 4.0
    complex :: z

    ! z = (x, y)      <-- 錯誤寫法！編譯器會報錯
    z = cmplx(x, y)   ! 正確寫法
    ```
    在進行高精度計算（如 complex(8)）時，建議寫成 cmplx(x, y, kind=dp)，以確保數值不會在轉換過程中損失精度。  
3. 範例
    ```fortran
    ! 1. 宣告也要加 kind
    complex(kind=dp) :: z

    ! 2. 賦值時務必加上 kind 參數
    z = cmplx(x, y, kind=dp) 

    ! 3. 如果是常數，則在數字後加上 _dp
    z = (1.23456789012345_dp, 0.0_dp)
    ```

<!--
#### **Integer**：計數器與位元運算   
#### **Complex**：複數運算與極座標轉換  
#### **Logical**：布林邏輯與短路評估  
#### **Character**：動態長度字串與 trim(), adjustl()
-->

### 1-2. 基礎陣列語法：陣列建構子與直接宣告

在 Fortran 中，如果你不需要 `allocatable` 的動態特性或 `parameter` 的常數保護，可以使用最純粹的陣列宣告與賦值方式。

#### 1-2-1. 陣列建構子 (Array Constructors)
這是在程式執行過程中，直接在程式碼裡「手寫」出一組數值的語法。

- **舊式語法 `(/ ... /)`**：相容性最強，所有版本皆支援。
- **新式語法 `[ ... ]`**：(Fortran 2003+) 語法更接近現代主流語言（如 Python、C#）。

**範例運用：**
```fortran
real :: vec(3)
vec = [1.0, 2.0, 3.0]                ! 使用中括號賦值
vec = (/ 1.0, 2.0, 3.0 /)            ! 使用舊式括號賦值（效果完全相同）

```

#### 1-2-2. 隱含式 Do 迴圈 (Implied-DO Loop)
這是 Fortran 的一大特色，讓你在宣告或賦值時，能用極短的程式碼生成規律的數值序列。

- **語法公式**：`[ (運算式, 變數 = 起始, 結束, 增量) ]`
**範例運用：**

```fortran
integer :: odds(5)
odds = [ (i, i = 1, 9, 2) ]          ! 生成 [1, 3, 5, 7, 9]

real :: squares(10)
squares = [ (real(i)**2, i = 1, 10) ] ! 生成 1 到 10 的平方數

```

#### 1-2-3. 單純陣列宣告 (不帶屬性)
最基本的陣列宣告法，其大小在編譯時就必須確定。

```fortran
real :: positions(3)                ! 一個長度為 3 的一維實數陣列
complex :: matrix(2, 2)             ! 一個 2x2 的二維複數陣列

```

#### 1-2-4. 基礎賦值與初始化

- **全體賦值**：`arr = 0.0` (將陣列中所有元素同時設為 0.0)
- **部分切片 (Slicing)**：`arr(1:3) = 1.0` (僅將前三個元素設為 1.0)

#### 1-2-5. 效能優化：記憶體順序與快取機制 (Cache Locality)

在 Fortran 中，程式碼的執行效率往往取決於你存取陣列的「路徑」是否符合底層硬體的物理結構。這涉及到電腦如何將數據從記憶體（RAM）搬運到 CPU 的快取中。

##### 1. 欄優先順序 (Column-Major Order)
雖然我們在數學上看矩陣是二維的，但在實體記憶體中，數據只能排成一條直線。Fortran 採用「欄優先」儲存方式，即**最左邊的索引（Index）變動最快**。

- **儲存邏輯**：先排完第一行，再排第二行。
- **範例**：假設矩陣 $A(2,2)$，在記憶體裡的實體順序是：`A(1,1) -> A(2,1) -> A(1,2) -> A(2,2)`。
- **對比**：C 語言或 Python (NumPy) 採用「列優先 (Row-Major)」，順序完全相反。

##### 2. 快取（Cache）的運作原理
CPU 為了加速，不會每次都去遠處的 RAM 拿一個數字。
- **Cache Hit (命中)**：當 CPU 讀取 `A(1,1)` 時，會順便將後面連在一起的一整塊數據搬進快取。如果你下一個讀取的是 `A(2,1)`，它就在快取裡，速度極快。
- **Cache Miss (失效)**：如果你跳著讀（例如先讀 `A(1,1)` 接著讀 `A(1,2)`），中間隔了整整一行。CPU 剛搬進來的快取沒用到就要丟掉，再去搬下一塊，速度會慢上 10 倍甚至百倍。

##### 3. 多維迴圈的「洋蔥規則」
為了最大化發揮硬體效能，撰寫嵌套迴圈時應遵守：**「最左邊的索引放在最內層」**。

- **正確範例 (快)**：
    ```fortran
    do j = 1, cols
        do i = 1, rows  ! 最左邊索引 i 在最內層，連續讀取
            A(i, j) = A(i, j) * 2.0
        end do
    end do
    ```
- **錯誤範例 (慢)**：
    ```fortran
    do i = 1, rows
        do j = 1, cols  ! 索引 j 在內層，造成記憶體跳躍存取
            A(i, j) = A(i, j) * 2.0
        end do
    end do
    ```

##### 4. 高維度延伸
這套規則適用於更高維度的陣列（Fortran 最高支援 15 維）。處理三維陣列 `A(i, j, k)` 時：
- `i` (最左)：變動最快，放最內層。
- `j`：中間層。
- `k` (最右)：變動最慢，放最外層。

##### 總結與建議
- **口訣**：左動快、內層放；右動慢、外層放。
- **物理意義**：保證 CPU 處理的數據在實體電路上是連在一起的。
- **自動化優化**：Fortran 的「全陣列操作」（如 `A = A * 2`）永遠是最安全的選擇，因為編譯器會自動選用該硬體架構下最正確、最快的順序。 

#### 1-2-6. 陣列存取與切片輸出 (Slicing)

在 Fortran 中，你可以透過括號內的冒號 `:` 靈活地選取陣列的一部分，這稱為「切片」。這在處理大型數據（如只取圖片的一部分或波形的前半段）時非常高效。

1. **`print *, arr(n:)` — 從 n 到最後**
   - **意義**：從索引 `n` 開始，印出到陣列的最後一個元素。
   - **特性**：冒號右側留空代表自動選取到該維度的上限 (Upper Bound)。
   - **範例**：如果 `arr` 是 `(1:10)`，`arr(8:)` 會印出第 8, 9, 10 個元素。

2. **`print *, arr(:n)` — 從最初到 n**
   - **意義**：從陣列的起始元素開始，印出到索引 `n` 為止。
   - **特性**：冒號左側留空代表自動從該維度的下限 (Lower Bound) 開始（Fortran 預設為 1）。
   - **範例**：`arr(:3)` 會印出第 1, 2, 3 個元素。

3. **`print *, arr(n)` — 單一元素選取**
   - **意義**：僅存取陣列中索引為 `n` 的單一元素。
   - **結果**：輸出會是一個**純量 (Scalar)** 數值，而不是一個陣列。

4. 語法總結表  
    | 語法 | 說明 | 範例 (假設 arr 為 1 到 10) |  
    | :--- | :--- | :--- |  
    | **`arr(:)`** | 全部元素 | 印出 1, 2, 3, ..., 10 |  
    | **`arr(n:)`** | 從 `n` 到最後 | `arr(8:)` 印出 8, 9, 10 |  
    | **`arr(:n)`** | 從最初到 `n` | `arr(:3)` 印出 1, 2, 3 |  
    | **`arr(n:m:s)`** | 從 `n` 到 `m`，步進 (間隔) 為 `s` | `arr(1:5:2)` 印出 1, 3, 5 |  

5. 專業小撇步
- **全選簡寫**：在 Fortran 中，`print *, arr` (不加括號) 效果完全等同於 `print *, arr(:)`，都會印出整個陣列。
- **多維應用**：切片同樣適用於多維陣列。例如 `matrix(:, 1)` 代表選取「所有列」的「第一行」，即取出矩陣的第一個直欄。
- **越界警告**：執行時，請求的索引範圍必須在宣告的 `dimension` 之內，否則會發生 **Out of Bounds** 錯誤導致程式崩潰。

#### 1-2-7. 注意事項

```fortran
real :: a(3)
a = [1.0, 2.0]  ! 錯誤！長度不符。
```

### 1-3. 精度控制 (kind) 與 selected_real_kind  
在科學計算中，精確度決定了模擬的可靠性。
#### 舊式 vs. 現代寫法
- **舊式 (不推薦)**  
    ```fortran
    real*8 :: x
    ```   
    依賴編譯器，不具移植性  
- **現代 (推薦)**   
    ```fortran
    ! 定義雙倍精確度 (Double Precision) 
    integer, parameter :: dp = selected_real_kind(15, 307)  
    real(kind=dp) :: x
    ```  
    這代表：需要一個能處理 **15 位有效數字**、範圍高達 **10的307次方** 的浮點數。
- `_dp` 與 `kind=8` 的區別  
    這涉及到電腦儲存數字的精確度（Precision）。
    - `kind` 的意義：電腦用多少個位元組（Byte）來存一個數字。`kind=4` 通常是單倍精確度（約 7 位有效數字），`kind=8` 是雙倍精確度（約 15 位有效數字）。
    - 為什麼不直接寫 `8`？：因為在不同的電腦系統上，`8` 不一定代表雙倍精確度（雖然 99% 的情況下是）。為了安全，我們會定義一個參數 `dp = selected_real_kind(15, 307)`，讓電腦幫我們找出最適合「15 位精度」的 `kind` 代碼。
    - `_dp` 的作用：當你寫 `1.0` 時，電腦預設它是單精度。如果你寫 `1.0_dp`，這是一個常數後綴，告訴電腦：「請用 `dp` 指定的高精度來儲存這顆浮點數。」這能避免在計算過程中因為精度損失而產生誤差。

#### 常數後綴 (Constants with Kind)
在運算中，如果寫 x = 1.0 + y，即便 y 是高精度，1.0 仍可能被視為低精度，導致誤差。  
- 正確寫法  
    ```fortran
    x = 1.0_dp + y
    ```

### 1-4. 定義屬性 (Attributes)

#### 1-4-1. 結構與狀態類

#### 1-1-6. parameter：編譯期常數（安全性與效能）
在 Fortran 中，`parameter` 屬性用於宣告一個**唯讀**的數值。這個數值在編譯時期就已經確定，且在程式執行過程中絕對無法被修改。

1. 核心語法
    ```fortran
    型別, parameter :: 常數名稱 = 數值
    型別, parameter :: 陣列名稱(*) = [數值1, 數值2, ...]
    ```

2. 核心特性
    - **安全性 (Security)**：防止程式碼在後續邏輯中意外修改到基礎物理常數。如果程式嘗試修改，編譯器會報錯攔截。
    - **效能 (Performance)**：編譯器知道這個值永遠不變，會直接將常數嵌入到計算指令中，省去記憶體讀取時間。
    - **(*) 陣列宣告**：當定義常數陣列時，可以使用 `(*)` 讓編譯器根據後方的初始值自動計算陣列大小，無需手動數個數。

3. 實戰應用範例
    ```fortran
    ! --- 標量常數 ---
    real, parameter :: pi = 3.14159265
    real, parameter :: c = 299792458.0        ! 光速 (m/s)
    real, parameter :: G = 9.80665            ! 重力加速度
    integer, parameter :: MAX_USERS = 50      ! 最大人數限制
    complex, parameter :: I = (0.0, 1.0)      ! 虛數單位 i

    ! --- 陣列常數 (*) ---
    ! 使用 (*) 讓編譯器自動偵測長度為 3
    real, parameter :: ORIGIN(*) = [0.0, 0.0, 0.0] 
    integer, parameter :: FIBONACCI(*) = [1, 1, 2, 3, 5, 8, 13]

    ! 也可以用於字串常數陣列
    character(len=*), parameter :: DAYS(*) = ["Mon", "Tue", "Wed", "Thu", "Fri"]
    ```

4. 進階技巧：常數表達式
    `parameter` 的數值可以使用「其他已經定義好的常數」來進行初始化計算：

    ```fortran
    real, parameter :: radius = 10.0
    real, parameter :: area = pi * radius**2   ! 合法，編譯時會自動算出結果
    ```

5. 使用禁忌與限制
    ```fortran
    real :: x = 5.0
    real, parameter :: y = x * 2.0  ! 錯誤！x 是變數，不能定義 parameter
    ```

6. 專業實務建議
    在大型專案中，通常會將所有的 `parameter` 集中放在一個名為 `Constants` 的 **Module** 中，然後在主程式中透過 `use` 指令來調用。這樣可以確保整套程式的物理常數與陣列大小設定完全統一，避免「幻數 (Magic Numbers)」散落在程式碼各處。

##### dimension：陣列維度與空間形狀宣告
`dimension` 屬性是 Fortran 定義資料結構「形狀」的核心。它告訴編譯器該變數不是單一數值（Scalar），而是一組連續存儲的資料集合。

1. **基本語法模板**
    - **靜態宣告**：  
        `型別, dimension(大小) :: 變數名`
        等同於 `型別 :: 變數名(大小)`
    - **多維宣告**：
        `型別, dimension(列, 行, 層, ...) :: 變數名` (Fortran 最高支援至 15 維)。

2. **核心特性**
    - **同質性**：陣列內的所有元素必須是同一型別（例如全是 `real` 或全是 `complex`）。
    - **指標存取**：使用括號 `()` 存取元素，例如 `points(1)`。注意：Fortran 預設索引是 **從 1 開始**，而非 0。
    - **常數陣列 (Parameter)**：結合 `parameter` 屬性可以定義不可變的常數清單，適合存放物理常數或固定幾何頂點。

3. **三種常見應用場景**

    **A. 基礎靜態陣列 (Static Array)**
    在編譯時就確定大小，適合已知規格的資料。
    ```fortran
    integer, dimension(10) :: student_ids     ! 10 個整數空間
    real, dimension(100) :: temperatures      ! 100 個實數空間
    complex, dimension(5) :: wave_functions   ! 5 個複數空間
    ```

    **B. 多維矩陣空間 (Multi-dimensional)**
    在科學計算中常用於座標系或網格點。
    ```fortran
    ! 定義一個 3x3 的旋轉矩陣
    real, dimension(3, 3) :: rotation_matrix

    ! 定義一個 100x100 的複數平面網格
    complex, dimension(100, 100) :: grid_points
    ```

    **C. 固定參數表 (Array Constants)**
    使用 `(/ ... /)` 或 `[ ... ]` 語法來初始化固定數值。
    ```fortran
    ! 定義三角形頂點 (3 個複數點)
    complex, dimension(3), parameter :: TRIANGLE = [ (0,0), (1,0), (0,1) ]

    ! 定義縮放因子
    real, dimension(2), parameter :: SCALE_FACTORS = [ 1.5, 3.2 ]
    ```

4. **進階控制：自定義索引邊界**
    `dimension` 不僅能定義大小，還能定義索引範圍（啟始:結束）。這在數學公式對應上非常有用：
    ```fortran
    ! 索引從 0 到 10，總共 11 個元素 (適合含原點的計算)
    real, dimension(0:10) :: x_coords 

    ! 索引從 -5 到 5
    real, dimension(-5:5) :: offset_data
    ```

5. **與 `allocatable` 的連結預告**
    當 `dimension` 括號內寫入具體數字（如 `10`），它是**靜態**的；當括號內寫入冒號（如 `:`），它就變成了**延後決定形狀**。這時必須搭配 `allocatable` 屬性，否則編譯器會報錯。

##### allocatable：動態陣列、自動回收與 allocated() 檢查

`allocatable` 是 Fortran 現代記憶體管理的靈魂。它將「定義維度（幾維）」與「分配空間（多大）」拆解開來，讓程式能處理未知大小的數據。

1. **語法公式：宣告 (Declaration)**
    - **規則**：括號內的 `dimension` 必須使用冒號 `:` 作為佔位符，代表維度已知但長度未定。
    - **一維陣列**：`real, allocatable :: arr(:)`
    - **二維矩陣**：`complex, allocatable :: grid(:, :)`
    - **三維張量**：`integer, allocatable :: tensor(:, :, :)`

2. **核心操作：三部曲 (Allocate -> Use -> Deallocate)**

    **A. 配置空間 (Allocate)**
    這是向作業系統申請記憶體的動作。
    ```fortran
    allocate(arr(n), stat=err_status) 
    ! n 可以是變數或運算結果。stat 可選，若配置失敗 err_status 會變為非零值。
    ```

    **B. 檢查狀態 (Allocated)**
    在配置前或釋放後，檢查陣列是否「有肉」的保險機制。
    ```fortran
    if (.not. allocated(arr)) then
        allocate(arr(100))
    end if
    ```

    **C. 手動釋放 (Deallocate)**
    將記憶體歸還系統。
    ```fortran
    if (allocated(arr)) deallocate(arr)
    ```

3. **核心特性與優勢**
    - **按需分配 (On-demand)**：完全根據輸入數據（如讀取檔案行數）決定記憶體大小，不浪費任何一個位元組。
    - **自動回收 (Auto-deallocation)**：
        - 如果 `allocatable` 陣列宣告在 `block` 內部，離開 `end block` 時會自動釋放。
        - 如果是子程式中的局部變數，執行完畢會自動釋放，大幅降低記憶體洩漏風險。
    - **自動調整大小 (Reallocation on Assignment)**：
        在 Fortran 2003 以後，如果你直接賦值給它，它會自動調整形狀：
        ```fortran
        real, allocatable :: v(:)
        v = [1.0, 2.0, 3.0]  ! 自動 allocate 長度為 3
        ```

4. **與 dimension 的關聯運用：動態座標點處理**
    這是在科學計算中最常見的場景：處理數量不定的座標轉換。
    ```fortran
    integer :: num_points
    complex, allocatable :: cloud_points(:)  ! 宣告一維動態陣列

    print *, "輸入點的總數："
    read *, num_points

    allocate(cloud_points(num_points))       ! 根據輸入分配空間
    
    ! ... 進行複數旋轉或縮放運算 ...
    ! cloud_points = cloud_points * rotator
    
    deallocate(cloud_points)                 ! 運算結束，釋放大型空間
    ```

5. **使用注意事項 (Best Practices)**
    - **嚴禁重複配置**：對已經 `allocated` 的陣列再次執行 `allocate` 會導致程式錯誤。
    - **效能考量**：雖然彈性極高，但 `allocate` 是相對耗時的操作，應避免在極高頻率的內層迴圈中反覆進行。
    - **多維一致性**：二維陣列 `allocate(m, n)` 時，`m` 為列長，`n` 為行長，必須嚴格遵守宣告時的維度數量。

#### 1-4-2. 參數傳遞類 (常用於子程式 Subroutine)

##### **intent(in / out / inout)**: 規定子程式中參數的流向。  
    - in: 唯讀，不可修改傳進來的變數。  
    - out: 唯寫，子程式會計算這個值並傳回。  
    - inout: 可讀可寫。
##### save：靜態變數（跨調用保留數值）

#### 1-4-3. 記憶體行為類 (進階)

##### **save**: 讓子程式中的局部變數在程式結束後「保存在記憶體中」，下次進入子程式時數值還在（類似 C 語言的 static）。  
##### **pointer**: 指標，指向另一個變數的記憶體位址。  
##### **target**: 標記該變數可以被 pointer 指向。

## 2. 邏輯控制與循環 (Control Flow)

### 2-1. 條件判斷與分支 (Conditions)

#### 2-1-1.IF：如果否則

##### Logical IF：單行極速判斷  
時機：只執行一件小事  
`if (temperature > 100.0) print *, "警告：水分已沸騰！"`  
不需要 then 也不需要 end if  

##### Block IF：多行結構  
時機：執行多行指令  
```fortran
if (x > 0.0) then
    print *, "正數"
else if (x < 0.0) then
    print *, "負數"
else
    print *, "這是零"
end if
```  

#### 2-1-2. Select Case：多路分支與範圍匹配

在處理複雜的離散數據或區間分類時，`select case` 的範圍匹配（Range Matching）功能非常強大，但需遵守嚴格的型別與邏輯規則。

##### 1. 範圍匹配規則
- **包含邊界**：使用冒號 `:` 定義區間時（如 `case (n:m)`），當變數的值等於 `n`、等於 `m`，或介於兩者之間時，該區塊皆會執行。
- **等價關係**：`case (1:3)` 在邏輯上等同於列舉所有數值 `case (1, 2, 3)`。
- **單向邊界（半開放區間）**：
    - **`case (a:)`**：包含 `a` 及其之後的所有值（大於等於 `a`）。
    - **`case (:b)`**：包含 `b` 及其之前的所有值（小於等於 `b`）。
    - 
##### 2. 核心特性與優勢
- **自動結束**：Fortran 的 `select case` 在執行完符合的 `case` 區塊後會自動跳出，不需要像 C 語言那樣手寫 `break`。
- **支援型別**：可用於 `integer` (整數)、`logical` (邏輯值) 以及 `character` (字元/字串)。
- **範圍匹配**：支援使用冒號 `:` 來定義閉區間。

##### 3. 型別限制與禁忌
- **型別限制**：範圍匹配語法僅適用於 **整數 (Integer)** 或 **字元 (Character)** 類型。
- **禁用實數**：**不能用於實數 (Real)**。由於浮點數存在精度誤差，Fortran 不允許在 `case` 中使用實數區間。
- **衝突警告**：在 Fortran 中，`select case` 的範圍 **不允許重疊**，你不能同時定義 `case (1:5)` 和 `case (5:10)`，因為數值 `5` 同時存在於兩個區塊中，這會導致編譯錯誤。
- **唯一性**：編譯器必須確保每一個輸入值都只能對應到唯一的一個 `case` 分支。

##### 4. 程式碼範例：分數與字元判斷
```fortran
! 範例 A：整數分數判定
select case (score)
    case (90:100)
        print *, "優秀"  ! 包含 90 和 100
    case (60:89)
        print *, "及格"  ! 包含 60 和 89
    case default
        print *, "不及格"
end select

! 範例 B：字元範圍判定
select case (grade_char)
    case ('A':'C')
        print *, "表現良好"
    case ('D', 'F')
        print *, "需要補考"
    case default
        print *, "無效等級"
end select

```

##### 語法總結表

| 語法 | 意義 | 範例 |
| --- | --- | --- |
| case (n) | 精確匹配單一值 | case (7) |
| case (n:m) | 閉區間 [n, m] | case (10:20) |
| case (n:) | 大於等於 n | case (60:) |
| case (:m) | 小於等於 m | case (:59) |
| case (a, b, c) | 離散清單匹配 | case (2, 4, 6) |

#### 2-1-3. 元素級選擇函數 (MERGE Function)
`merge` 是一個內建函數，它像是一個向量化的 `if-else` 表達式，根據條件從兩個來源中挑選元素。

##### 1. 語法公式
```fortran
result = merge(tsource, fsource, mask)
```
- **tsource**：當 `mask` 為真時取用的值/陣列。
- **fsource**：當 `mask` 為假時取用的值/陣列。
- **mask**：邏輯遮罩（可以是一個邏輯值或邏輯陣列）。

##### 2. 核心特性
- **表達式嵌入**：因為它是函數，可以直接寫在賦值語句的右側或傳遞給其他函數。
- **同步計算**：注意！在某些編譯器中，`tsource` 和 `fsource` 可能都會先被計算出來，再由 `merge` 挑選，這與 `where` 的跳轉機制略有不同。

##### 3. 精要範例

```fortran
! 將負數設為 0，正數保持不變 (單行解決)
A = merge(A, 0.0, A > 0.0)

! 根據標誌決定輸出字串
print *, merge("通過", "失敗", score >= 60)
```

### 2-2. 迴圈控制 (Loops)

Fortran 提供多種迴圈機制，從傳統的計數循環到現代的並行運算優化。

#### 1. 核心迴圈三者對比表
這三種迴圈分別對應不同的應用場景，選擇正確的語法能顯著提升程式的可讀性與執行效能。

| 特性 | **Do Loop** | **Do While** | **Do Concurrent** |
| :--- | :--- | :--- | :--- |
| **執行次數** | 預先確定 | 視條件而定 | 預先確定 |
| **並行潛力** | 中（需依賴編譯器分析） | 低 | 極高（語義保證） |
| **主要用途** | 標準迭代、陣列遍歷 | 邏輯控制、迭代收斂（如誤差判斷） | 高效能計算、自動向量化 |
| **標準引入** | Fortran 77 (或更早) | Fortran 77/90 | Fortran 2008 |

#### 2. Do Loop：計數步進循環
這是最基礎的迴圈，用於處理已知次數的重複任務。
- **語法**：`do 變數 = 起始, 結束, [增量]`
- **細節**：
    - 若未提供「增量」，預設為 `1`。
    - 增量可為負數，用於遞減循環。
    ```fortran
    do i = 1, 10, 2
        print *, "奇數系列：", i  ! 印出 1, 3, 5, 7, 9
    end do
    ```

#### 3. Do While：條件式循環
當你不知道要執行幾次，只知道「何時該停」時使用。
- **語法**：`do while (邏輯條件)`
- **實戰範例**：常用於數值分析中的收斂判斷。
    ```fortran
    do while (abs(error) > 1.0e-7)
        ! ... 反覆計算 x ...
        error = x_new - x_old
    end do
    ```

#### 4. Do Concurrent：並行語義循環
這是現代 Fortran 的強大工具，它告訴編譯器：**每次循環都是獨立的**，舊版名稱：`forall`。
- **核心優勢**：編譯器可以放心地將運算分配到不同的 CPU 核心或使用 SIMD 指令集。
- **限制**：區塊內不得有 `exit`、`return` 或 `io` 操作（如 `print`, `write`），因為這會破壞並行性。
    ```fortran
    do concurrent (i = 1:n, j = 1:m)
        matrix(i, j) = exp(real(i+j))
    end do
    ```

#### 5. 迴圈跳轉控制：exit 與 cycle
這兩個指令提供了更細緻的邏輯流向控制。
- **`exit`**：立即中斷並退出目前所在的迴圈。
- **`cycle`**：跳過當前這一次的後續指令，直接進行下一次循環。
- **具名迴圈（Named Loops）**：建議在巢狀迴圈中使用，可增加可讀性並精準控制跳轉。
    ```fortran
    search_loop: do i = 1, N
        if (is_invalid(i)) cycle search_loop  ! 發現無效值，跳過本次處理下一筆
        if (is_found(i)) exit search_loop     ! 找到目標，直接終止整個搜尋
    end do search_loop
    ```

#### 專業建議
- **避免無限迴圈**：使用 `do while` 時，務必確保循環內部的變數最終能使條件變為 `.false.`。
- **優先使用向量化**：如果是在對整個陣列做加減乘除，直接使用 `A = B + C`（向量化運算）通常比手寫 `do loop` 更快且更簡潔。

## 3. 核心運算與陣列 (Computation & Arrays)

### 3-1. 算術運算子 (Arithmetic Operators)
| 運算子 | 說明 | 範例 | 優先級 |
| :---- | :---- | :---- | :---- |
| `**` | 乘冪 (Power) | `x**3` (即 $x^3$ ) | 1 (最高) |
| `*` | 乘法 | `a*b` | 2 |
| `/` | 除法 | `a/b` | 2 |
| `+` | 加法 | `a+b` | 3 |
| `-` | 減法 (或負號) | `-a+b` | 3 |  

**注意點：**
- **整數除法**：若兩者皆為整數，結果會無條件捨去小數（例如 `7/2` 結果為 3）。  
- **乘冪限制**：`(-2.0)**2.0` 會報錯，因為實數底數不能為負（需用整數指數 `(-2.0)**2`）。

### 3-2. 關係運算子 (Relational Operators)
用於比較兩個值，結果返回邏輯值 (`.TRUE.` 或 `.FALSE.`)。  
| 新式符號 | 舊式縮寫 | 說明 |
| :---- | :---- | :---- |
| `==` | `.EQ.` | 等於 (Equal to) |
| `/=` | `.NE.` | 不等於 (Not Equal to) |
| `>` | `.GT.` | 大於 (Greater Than) |
| `<` | `.LT.` | 小於 (Less Than) |
| `>=` | `.GE.` | 大於等於 (Greater or Equal) |
| `<=` | `.LE.` | 小於等於 (Less or Equal) |

### 3-3. 邏輯運算子 (Logical Operators)**
用於結合多個布林表達式，**前後必須加點**。
| 運算子 | 說明 | 優先級 |
| :---- | :---- | :---- |
| `.NOT.` | 邏輯非 (NOT) | 1 |
| `.AND.` | 邏輯與 (AND) | 2 |
| `.OR.` | 邏輯或 (OR) | 3 |
| `.EQV.` | 邏輯等價 (Equivalence) | 4 |
| `.NEQV.` | 邏輯不等價 (XOR) | 4 |

### 3-4. 字串運算子 (Character Operator)**
| 運算子 | 說明 | 範例 |
| :---- | :---- | :---- |
| `//` | 字串連接 (Concatenation) | `'Fort' // 'ran'` → `'Fortran'` |

### 3-5. 數學函數與精準度 (Math Library)

#### 基本運算：次方 (**)、模數 (mod)、餘數 (modulo)  
#### 三角函數：sin, cos, atan2 (處理座標象限)  
#### 複數運算與極座標轉換
##### 複數相關函數
| 函數 | 數學意義 | 物理/幾何用途 |
| :---- | :---- | :---- |
| `real(z)` | 實部 $Re(z)$ | 取得 X 座標 |
| `aimag(z)` | 虛部 $Im(z)$ | 取得 Y 座標 |
| `abs(z)` | 模長 $\|z\|$ | 計算幅值、振幅、距離原點距離 |
| `conjg(z)` | 共軛複數 $x - iy$ | 電磁波反射 / 矩陣運算 |
| `cmplx(x, y, kind)` | 組合 $x + iy$ | 座標封裝 |

##### 在複數平面上旋轉
- 在 Fortran 中，旋轉向量 (x, y) 角度 $\theta$：  
    ```fortran
    ! 1. 建立旋轉因子 (單位圓上的點)
    rotator = cmplx(cos(theta), sin(theta), kind=dp)

    ! 2. 旋轉所有點
    new_points = old_points * rotator
    ```
    這比手寫 $x' = x\cos\theta - y\sin\theta$ 更不容易出錯，且語法更接近物理本質。

- 數學邏輯：複數平面
    這是最精彩的部分！為什麼「相乘」等於「旋轉」？
    - 歐拉公式：$e^{i\theta} = \cos\theta + i\sin\theta$。
    - 複數的極座標形式：任何一個點 $(x, y)$ 都可以表示成 $z = r \cdot e^{i\phi}$，其中 $r$ 是長度，$\phi$ 是與 X 軸的夾角。
    - 旋轉的魔術：
        1. 假設有一個旋轉因子 $R = e^{i\theta}$（這是一個長度為 1，角度為 $\theta$ 的向量）。
        2. 將原本的點 $z$ 乘以 $R$：
            $$z' = z \cdot R = (r \cdot e^{i\phi}) \cdot (e^{i\theta}) = r \cdot e^{i(\phi + \theta)}$$
        3. 看！結果的長度 $r$ 沒變，但角度變成了 $\phi + \theta$。這就是旋轉！

- 範例：
```fortran
program complex_rotation
  implicit none
  
  ! 1. 定義雙倍精確度 (dp)，確保計算精準至 15 位有效數字
  ! selected_real_kind(15, 307) 代表至少 15 位有效數字，指數範圍到 10^307
  integer, parameter :: dp = selected_real_kind(15, 307)
  real(dp), parameter :: PI = 3.141592653589793_dp ! 注意 _dp 後綴
  
  ! 2. 宣告複數變數
  complex(dp) :: point, rotator, result
  real(dp) :: angle_deg, angle_rad
  
  ! 設定初始點為 (1, 0)。1.0_dp 確保數字從一開始就是高精確度
  point = (1.0_dp, 0.0_dp)
  
  print *, "=== 複數旋轉實驗 ==="
  print *, "原始座標 (x, y):", real(point), ",", aimag(point)
  
  ! 3. 設定旋轉角度（例如逆時針旋轉 90 度）
  angle_deg = 90.0_dp
  angle_rad = angle_deg * PI / 180.0_dp 
  
  ! cmplx(x, y, kind) 是建構複數的標準函數
  rotator = cmplx(cos(angle_rad), sin(angle_rad), kind=dp)
  
  ! 4. 數學邏輯：z' = z * e^(i*theta)
  ! 在複數平面上，這相當於座標旋轉
  result = point * rotator
  
  print *, "旋轉 ", angle_deg, " 度後的結果："
  print *, "新座標 (x, y):", real(result), ",", aimag(result)
  
  ! 陣列威力：同時旋轉多個點
  block
    complex(dp), dimension(3) :: triangle
    ! 使用 (/ ... /) 建構陣列，裡面可以放複數
    triangle = (/ (0.0_dp, 0.0_dp), (1.0_dp, 0.0_dp), (0.0_dp, 1.0_dp) /)
    
    print *, ""
    print *, "=== 批量旋轉三角形頂點 ==="
    ! 不需要迴圈，直接與旋轉因子相乘
    triangle = triangle * rotator
    print *, "旋轉後的三角形頂點 2:", triangle(2)
    print *, "旋轉後的三角形頂點 3:", triangle(3)
  end block

end program complex_rotation
```

#### Logical：布林邏輯與短路評估  
#### Character：動態長度字串與 trim(), adjustl()
#### 線性代數：dot_product (內積), matmul (矩陣乘法)

### 3-6. 陣列切片與向量化 (Array Ops)

Fortran 的核心優勢在於「向量化思維」，即直接對整個陣列進行運算，而不必撰寫繁瑣的迴圈。

#### 3-6-1. 陣列切片進階運用 (Slicing)
透過冒號 `:` 語法，可以精準控制資料子集的提取與賦值。
- **語法公式**：`陣列名(起始索引 : 結束索引 : 步進間隔)`
- **範例應用**：
    ```fortran
    real :: A(10) = [ (real(i), i=1, 10) ]
    
    A(1:5) = 0.0             ! 前 5 個元素歸零
    A(::2) = A(::2) * 2.0    ! 所有「奇數位置」的元素翻倍
    A(10:1:-1)               ! 陣列反轉輸出
    ```

#### 3-6-2. 全陣列運算 (Whole-Array Operations)
只要陣列形狀（Shape）相同，即可直接進行算術運算。
```fortran
! 假設 A, B 是相同大小的陣列
C = A + B       ! 對應元素相加
D = A * B       ! 對應元素相乘 (注意：非矩陣乘法)
E = A > 0.5     ! 返回相同大小的邏輯陣列

```

#### 3-6-3. 條件遮罩運算 (WHERE Construct)
`where` 專門用於對陣列進行「批次條件賦值」。它會根據一個邏輯遮罩（Mask），決定哪些元素需要執行特定運算。

1. 語法公式
    ```fortran
    where (邏輯遮罩陣列)
        ! 遮罩為 .true. 時執行的陣列運算
    [elsewhere (新邏輯遮罩)]
        ! 遮罩為 .true. 時執行的陣列運算
    [elsewhere]
        ! 其餘元素執行的陣列運算
    end where
    ```

2. 核心特性
   - **區塊操作**：適用於多行賦值運算。
   - **保護機制**：常用於防止非法運算（如除以零或對負數開根號）。
   - **效能**：編譯器通常會對 `where` 塊進行向量化優化。

3. 精要範例
    ```fortran
    where (A /= 0.0)
        B = 1.0 / A         ! 只對非零元素做倒數
    elsewhere
        B = -1.0            ! 零元素設為標記值
    end where
    ```

#### 3-6-4. 元素級選擇與陣列縮減 (Elemental Selection & Reduction)
在 Fortran 中有一系列強大的內建函數，能根據邏輯遮罩（Mask）對陣列進行過濾、壓縮或狀態判斷。這些工具通常支援向量化（Vectorization），效能極高。

##### 1. pack (條件式壓縮)
如果 `merge` 是「二選一」，那麼 `pack` 就是「過濾器」。它會根據遮罩把符合條件的元素挑出來，組成一個新的、一維的短陣列。

```fortran
integer :: A(5) = [1, -2, 3, -4, 5]
print *, pack(A, A > 0)  ! 輸出：1, 3, 5
```

##### 2. unpack (條件式展開)
這是 `pack` 的逆向操作。它把一個短陣列（向量），按照指定的遮罩，散佈回一個大陣列的特定位置中，其餘位置填入預設值。

- **語法**：`result = unpack(vector, mask, field)`

##### 3. any 與 all (邏輯縮減)
當你需要將整個陣列的判斷縮減為一個單一的 `.true.` 或 `.false.` 時使用。

```fortran
if (any(A < 0)) print *, "警告：陣列中包含負數！"
```

##### 4. minloc / maxloc (位置搜尋)
回傳符合條件的元素在陣列中的**索引位置**（Index），而非數值本身。

```fortran
integer :: B(3) = [10, 50, 20]
print *, maxloc(B)  ! 輸出：2 (因為 50 是最大值，位在第 2 個)
```

##### 5. 陣列邏輯工具箱總結
這張表彙整了在科學運算中處理陣列邏輯的最強工具：

| 功能類型 | 函數/指令 | 核心目的 | 適用時機 |
| --- | --- | --- | --- |
| 條件選擇 | merge | 簡單的 A 或 B 二選一。 | 表達式內部的小型判斷。 |
| 資料過濾 | pack / unpack | 篩選符合條件的子集合。 | 排除無效數據、壓縮稀疏矩陣。 |
| 批次處理 | where | 對陣列中符合條件的部分進行運算。 | 大型矩陣的條件賦值與計算。 |
| 位置檢索 | minloc / maxloc | 尋找極值的座標。 | 需要知道「誰」最大/最小時。 |
| 邏輯檢查 | any / all | 判斷整個陣列的狀態。 | if 語句中的陣列整體判斷。 |

#### 3-6-5. 陣列內建函數速查表
以下函數皆支援「全陣列」輸入與向量化運算。掌握選用參數 `dim` 與 `mask` 能讓你精準控制運算範圍（例如：只計算每一列的總和）。

##### 1. 屬性查詢 (Inquiry)

| 函數名 | 功能說明 | 常用範例 |
| --- | --- | --- |
| size(array, [dim]) | 取得元素總數或指定維度的大小 | n = size(A) / cols = size(A, 2) |
| shape(source, [kind]) | 取得陣列形狀（回傳各維度大小的整數陣列） | dims = shape(matrix) |
| lbound(array, [dim]) | 取得陣列的最小下界索引 | low = lbound(A, 1) |
| ubound(array, [dim]) | 取得陣列的最大上界索引 | high = ubound(A, 1) |

##### 2. 幾何重塑 (Geometric)

| 函數名 | 功能說明 | 常用範例 |
| --- | --- | --- |
| reshape(source, shape, [pad], [order]) | 改變維度（如 1D 轉 2D），pad 可補足缺值 | B = reshape(A, [2, 5]) |
| transpose(matrix) | 二維矩陣轉置（行列互換） | B = transpose(A) |
| spread(source, dim, ncopies) | 增加維度並複製數據（類似張量擴張） | B = spread(vec, 1, 3) |

##### 3. 數據歸約 (Reduction)
這些函數通常帶有 `dim`（指定維度計算）與 `mask`（過濾特定條件）參數。

| 函數名 | 功能說明 | 常用範例 |
| --- | --- | --- |
| sum(array, [dim], [mask]) | 計算總和 | row_sum = sum(A, dim=2) |
| maxval(array, [dim], [mask]) | 找出最大值 | peak = maxval(A, mask=A < 100) |
| minval(array, [dim], [mask]) | 找出最小值 | floor = minval(A) |
| any(mask, [dim]) | 只要有一個為真，即回傳 .true. | if (any(A < 0)) stop |
| all(mask, [dim]) | 全部皆為真，才回傳 .true. | if (all(A > 0)) ... |
| product(array, [dim], [mask]) | 計算所有元素的乘積 | p = product(A) |

##### 4. 線性代數與位置 (Linear Algebra & Location)

| 函數名 | 功能說明 | 常用範例 |
| --- | --- | --- |
| matmul(matrix_a, matrix_b) | 矩陣乘法（數學上的 A×B） | C = matmul(A, B) |
| dot_product(vector_a, vector_b) | 兩向量的內積（點積） | res = dot_product(u, v) |
| maxloc(array, [mask], [dim]) | 找出最大值的位置索引（回傳座標） | pos = maxloc(A) |
| minloc(array, [mask], [dim]) | 找出最小值的位置索引 | pos = minloc(A) |

##### 關鍵參數解析：`dim` 的威力
在歸約函數中使用 `dim` 參數，可以讓你從「塌陷成一個數」變成「按列/行運算」。

```fortran
real :: A(2, 3) = reshape([1, 2, 3, 4, 5, 6], [2, 3])
! A 是:
! 1  3  5
! 2  4  6

print *, sum(A)          ! 輸出 21 (全部相加)
print *, sum(A, dim=1)   ! 輸出 [3, 7, 11] (每一列加總，即 1+2, 3+4, 5+6)
print *, sum(A, dim=2)   ! 輸出 [9, 12]    (每一行加總，即 1+3+5, 2+4+6)

```

##### 專業建議

- **效能考量**：`matmul` 是經過編譯器高度優化的函數，處理矩陣相乘時，效能遠比自己寫三層 `do` 迴圈快得多。
- **類型一致**：`dot_product` 與 `matmul` 要求輸入陣列的數據型別必須一致（例如皆為 `real` 或皆為 `integer`

## 4. 數據交換 (I/O & Files)

### 4-1. 標準輸入與輸出 (Standard I/O)

#### 4-1-1. print 語法：標準輸出

`print` 是 Fortran 中最直觀、最快速的輸出指令，主要用於將資料顯示在螢幕（標準輸出設備）上。

1. **基本語法公式**
    `print 格式控制, 輸出清單`

2. **核心用法解析**
    * **自由格式 (List-Directed I/O)**：使用星號 `*` 代表由編譯器根據資料型別自動決定顯示方式。
    * **輸出清單**：可以使用逗號 `,` 隔開多個不同型別的項目，包含字串常數、變數或表達式。

3. **實戰應用範例**
    ```fortran
    integer :: count = 5
    real :: gravity = 9.81
    
    ! 1. 混合輸出字串與變數
    print *, "當前計數：", count, " 重力加速度：", gravity
    
    ! 2. 輸出表達式計算結果
    print *, "兩倍重力：", gravity * 2.0
    
    ! 3. 空行輸出
    print * ! 僅印出一個換行符號
    ```

4. **陣列輸出特性**
    * **全陣列印出**：直接使用陣列名稱而不加括號，效果等同於印出 `arr(:)`。
    * **空間分隔**：預設情況下，`print *` 會在陣列元素之間自動加入空格（或換行，視資料長度而定）。
    ```fortran
    real :: arr(3) = [1.0, 2.0, 3.0]
    print *, arr          ! 印出: 1.000000 2.000000 3.000000
    print *, "第一項：", arr(1)
    ```

5. **專業小撇步**
    * **除錯必備**：在開發過程中，`print *, "Check 1: ", var` 是最常用的追蹤方法。
    * **橫向合併**：若要在一行內印出規律數據，可搭配 **隱含式 Do 迴圈**：
      `print *, (i*i, i=1, 5)` $\rightarrow$ 印出 `1 4 9 16 25`

#### 4-1-2. write 語法：格式化與檔案輸出

與 `print` 不同，`write` 指令可以精確指定輸出設備（螢幕或檔案）並完全掌控排版格式。

1. **基本語法公式**
    `write(unit, format, [options]) 輸出清單`
    - **unit**：設備編號。`*` 代表螢幕，整數（如 `10`）代表已開啟的檔案。
    - **format**：格式控制。`*` 代表自由格式，也可以是標號（如 `100`）或字串（如 `"(F8.2)"`）。
        - 當你需要一次輸出多個變數，且每個變數的格式不同時，可以在 `write` 的格式字串（Format String）中用 **逗號** 隔開多個格式代碼。  
            ```fortran
            write(*, '(A5, I3, F6.2)') label, id, pi
            ```
        - 如果有連續多個相同類型的資料，不需要重複寫，直接在代碼前加上 **數字**，（括號可用於分組重複）。  
            `3F10.2` 等同於 `F10.2, F10.2, F10.2`   
            `(2(I3, F6.1))` 等同於 `I3, F6.1, I3, F6.1`  
            ```fortran
            real :: arr(3) = [1.1, 2.2, 3.3]
            write(*, '(3F5.1)') arr  ! 一口氣印出三個 5.1 格式的浮點數 
            ```
        - 如果格式太長，寫在 `write` 裡面會很亂，傳統做法是給它一個標號 (Label)。
            ```fortran
            WRITE(*, 100) A, B
            100 FORMAT(I5, F8.2)
            ```

2. **核心用法範例**
    ```fortran
    integer :: age = 25
    real :: pi = 3.14159
    
    ! A. 輸出至螢幕（等同於 print *）
    write(*, *) "年齡：", age
    
    ! B. 使用指定的格式標號
    write(*, 100) pi
    100 format("圓周率精確到兩位：", F5.2)
    
    ! C. 寫入特定檔案 (假設檔案已 open 為 unit 10)
    write(10, *) "這是寫入檔案的一行文字。"
    ```

3. write 指令的常用控制參數 [options]  
    在 `write(unit, format, [options])` 中，這些選用參數能讓你處理錯誤、控制換行，以及實現高效的資料流管理。 

    - advance：控制換行行為
        這是現代 Fortran 最常用的參數，決定輸出後游標是否移至下一行。
        - **`advance='yes'`** (預設)：輸出後自動換行。
        - **`advance='no'`**：輸出後游標停在原處，不換行。
        - **應用場景**：製作互動式輸入提示、動態進度條。
            ```fortran
            write(*, '(A)', advance='no') "正在計算中，請稍候... "
            ! ... 計算邏輯 ...
            write(*, '(A)') "完成！"
            ```

    - iostat：錯誤與狀態偵測  
        將執行狀態碼存入一個整數變數中，防止程式因輸出錯誤而直接崩潰。
        - **`iostat = stat_var`**：
            - `stat_var == 0`：執行成功。
            - `stat_var > 0`：發生錯誤（如磁碟空間不足、寫入權限受阻）。
            - `stat_var < 0`：遇到檔案結尾 (EOF)。
            ```fortran
            integer :: ios
            write(10, *, iostat=ios) data_array
            if (ios /= 0) print *, "寫入錯誤，錯誤代碼：", ios
            ```

    - iomsg：自定義錯誤訊息  
        搭配 `iostat` 使用，將錯誤原因存入一個字串變數中，方便除錯。
        - **用法**：`iomsg = msg_var`
            ```fortran
            character(len=100) :: msg
            write(20, *, iostat=ios, iomsg=msg) x
            if (ios /= 0) print *, "錯誤訊息：", trim(msg)
            ```

    - rec：隨機存取 (Random Access)  
        當檔案以「直接存取 (Direct Access)」模式開啟時，用來指定要寫入哪一筆紀錄（Record）。
        - **用法**：`rec = record_number`
            ```fortran
            ! 寫入檔案中的第 5 筆紀錄
            write(15, '(F10.2)', rec=5) val
            ```

    - pos：流式存取位置 (Stream I/O)  
        在 Fortran 2003 引入的流式存取模式中，指定寫入的位元組位置。
      - **用法**：`pos = byte_position`

    - [options] 參數彙整表

        | 參數 | 接受值 | 主要用途 |
        | :--- | :--- | :--- |
        | **`advance`** | `'yes'`, `'no'` | 是否在輸出結束後自動換行。 |
        | **`iostat`** | 整數變數 | 捕捉執行狀態碼，避免程式因報錯終止。 |
        | **`iomsg`** | 字串變數 | 存儲具體的錯誤描述文字。 |
        | **`err`** | 標號 (Label) | 發生錯誤時，程式跳轉到的指定行號。 |
        | **`rec`** | 正整數 | 直接存取檔案時，指定寫入的紀錄編號。 |
        | **`asynchronous`**| `'yes'`, `'no'` | 指定是否進行非同步輸出（加速大型運算）。 |

    - 專業建議：健壯性寫法
        在開發正式軟體或處理大量數據寫入時，建議養成使用 `iostat` 的習慣：
        ```fortran
        write(unit=u, fmt='(10F12.4)', iostat=io_status) array
        if (io_status /= 0) then
            ! 執行錯誤處理邏輯，例如關閉檔案或備份數據
        end if
        ```

4. **write vs. print 深度對比表**

    | 特性 | **print** | **write** |
    | :--- | :--- | :--- |
    | **目標設備** | 固定為標準輸出 (螢幕) | 可選螢幕 `*` 或檔案 `unit` |
    | **控制力** | 低，僅簡單輸出 | 高，支援 `advance`, `iostat`, `rec` 等 |
    | **非換行輸出** | 不支援 | 支援 (`advance='no'`) |
    | **內部轉換** | 不支援 | 支援 (數值轉字串) |
    | **格式自訂** | 支援，但較少搭配複雜 format | 標準用法，常搭配 format 語句 |

5. **專業小撇步**
    * **安全性**：在商業或大型科學計算中，建議始終搭配 `iostat`，以防止程式因為磁碟空間不足或權限問題突然崩潰。
    * **格式字串**：對於簡單的格式，可以直接寫在 write 裡面，例如 `write(*, "(I5)") count`，這樣可以省去寫 `format` 標號的麻煩。

#### 4-1-3. read 語法：標準輸入與數據讀取

`read` 指令用於從鍵盤（標準輸入）或檔案中讀取數據。與 `write` 相似，它提供了強大的狀態偵測機制來處理非預期的輸入格式。

1. **基本語法公式**
    `read(unit, format, [options]) 變數清單`
    - **unit**：`*` 代表鍵盤輸入，整數代表檔案。
    - **format**：`*` 代表自由格式（最常用），會自動根據空格或換行拆分數據。

2. **核心用法範例**
    ```fortran
    integer :: age
    real :: height
    character(len=20) :: name

    ! A. 基礎鍵盤輸入 (使用者輸入時可用空格或逗號分隔)
    print *, "請輸入姓名、年齡與身高："
    read(*, *) name, age, height

    ! B. 從檔案讀取一整行
    read(10, *) data_val
    ```

3. **關鍵控制參數 [options]**
    - **`iostat=var`**：**極其重要！** 捕捉讀取狀態。
        - `0`：讀取成功。
        - `> 0`：發生錯誤（例如：原本要讀整數，使用者卻輸入了字母）。
        - `< 0`：讀到檔案結尾 (End of File, EOF)。
    - **`end=label`**：讀到檔案末尾時，程式跳轉到指定的行號。
    - **`err=label`**：讀取發生錯誤時，跳轉到指定的行號。

4. **實戰：健壯的輸入檢查**
    為了避免使用者亂輸入導致程式崩潰，專業的寫法如下：
    ```fortran
    integer :: num, ios
    do
        print *, "請輸入一個整數："
        read(*, *, iostat=ios) num
        if (ios == 0) exit  ! 讀取成功，跳出迴圈
        print *, "錯誤：輸入格式不正確，請重新輸入。"
    end do
    ```

5. **內部檔案讀取 (字串轉數值)**
    如同 4-1-5 所述，`read` 可以直接操作字串：
    ```fortran
    character(len=10) :: str = "123.45"
    real :: val
    read(str, *) val  ! 將字串 "123.45" 轉換為實數 123.45
    ```

6. **專業小撇步**
    - **自由格式的威力**：使用 `read(*, *)` 時，Fortran 會自動忽略多餘的空格與換行，直到填滿變數清單所需的資料量為止。
    - **字串讀取的陷阱**：讀取字串時，若輸入內容包含空格，建議使用格式控制 `(A)` 或先讀取整行再處理，否則 `read(*, *)` 可能會因為空格而將字串切斷。
    - **iostat 代碼表**：不同的編譯器（Intel, GNU）對正值的錯誤代碼定義不同，但 `0` 與負值（EOF）是通用的標準。

#### 4-1-5. 內部檔案 (Internal Files)：字串與數值互轉

在 Fortran 中，`write` 和 `read` 不僅能操作螢幕和檔案，還能操作「字串變數」。這被稱為內部檔案處理，是實現**數值 (Numeric) 與字串 (Character) 互相轉換**的最標準作法。

##### 1. 數值轉字串 (Numeric to Character) — 使用 `write`
當你需要將計算結果嵌入到特定格式的字串中（例如自動生成檔名 `data_001.txt`）時使用。

- **語法**：`write(字串變數, 格式碼) 數值變數`
- **範例**：
    ```fortran
    character(len=20) :: str
    real :: val = 12.34
    integer :: file_idx = 7
    
    ! 將實數轉為字串
    write(str, "(F5.2)") val    ! str 內容變為 "12.34"
    
    ! 實務應用：動態生成補零的檔名
    character(len=12) :: filename
    write(filename, "('file_', I3.3, '.dat')") file_idx  ! filename 變為 "file_007.dat"
    ```

##### 2. 字串轉數值 (Character to Numeric) — 使用 `read`
當你從外部讀入一串文字，需要將其中的數字提取出來進行運算時使用。這類似於 C 語言的 `sscanf`。

- **語法**：`read(字串變數, 格式碼) 數值變數`
- **範例**：
    ```fortran
    character(len=20) :: input_str = "  456.78"
    real :: target_val
    integer :: target_int
    
    ! 從字串中讀取實數
    read(input_str, "(F8.2)") target_val  ! target_val 變為 456.78
    
    ! 從字串中讀取整數 (自動忽略空格)
    read(input_str, "(I8)") target_int    ! target_int 變為 456
    ```

##### 3. 核心優勢與應用場景
- **型別安全轉換**：Fortran 沒有 `itoa` 或 `atof` 等函數，`read/write` 到內部檔案是唯一的標準轉換方式。
- **複雜格式解析**：可以先用 `read` 讀入一整行字串，檢查內容後，再用內部檔案讀取選定部分，避免程式因輸入格式錯誤而崩潰。
- **動態 UI/報表**：在印出訊息前，先在字串中排版，確認無誤後再一次印出。

##### 專業小撇步
- **長度檢查**：使用 `write` 轉字串時，目標字串變數的長度（len）必須足以容納格式碼定義的大小，否則會發生執行時錯誤。
- **修剪空格**：讀取字串前，建議搭配 `adjustl()`（靠左對齊）或 `trim()`，確保格式碼能精準對齊數字起始位置。
    ```fortran
    ! 安全的轉換寫法
    read(adjustl(input_str), *) target_val
    ```

### 4-2.  格式化控制 (Format Control)

基礎格式碼：整數 I, 實數 F, 指數 E, 字串 A

排版技巧：跳格 X, 定位 T, 換行 /

隱含 Do 迴圈：在一行內寫入整個陣列 (A(i), i=1, n)

### 4-3. 外部檔案操作 (File Management)

在 Fortran 中，處理檔案遵循「開啟、操作、關閉」的嚴謹流程，並透過「單位編號 (Unit)」進行代號化管理。

#### 4-3-1. Open & Close：單位編號與資源管理

在 Fortran 中，所有的檔案操作都必須先透過 `open` 建立檔案與「單位編號 (Unit Number)」的連結。

##### 1. 語法公式
```fortran
! 開啟檔案
open([unit=]編號, file='檔名', [options])

! 關閉檔案
close([unit=]編號, [status='keep'/'delete'])

```

##### 2. 單位編號 (Unit Number) 的選取

```fortran
integer :: u
open(newunit=u, file='output.dat', status='replace')
! 此時 u 會被自動賦值（例如 -10），後續讀寫皆使用 u
write(u, *) "Hello Fortran!"
close(u)

```

##### 3. Close 指令的細節

```fortran
close(u, status='keep')

```

##### 專業小撇步

- **自動關閉**：雖然程式結束時通常會自動關閉所有檔案，但手動 `close` 是一個優良的習慣，能防止在長時間運算的程式中累積過多開啟的檔案。
- **隱含編號**：如果你使用 `print *`，實際上它等同於 `write(6, *)`。

#### 4-3-2. Open 關鍵參數速查表

`open` 指令的參數決定了程式如何與作業系統請求檔案資源。正確設定這些參數能大幅提升程式的健壯性與資料安全性。

##### 1. 核心參數詳解表

| 參數 | 接受值 (常用) | 詳細說明與行為 |
| :--- | :--- | :--- |
| **`status`** | `'old'` | **讀取專用**：檔案必須存在，否則報錯。 |
| | `'new'` | **安全建立**：建立新檔，若檔案已存在則報錯（防止覆蓋重要數據）。 |
| | `'replace'` | **強制覆蓋**：若檔案存在則刪除並重建；若不存在則直接建立。 |
| | `'scratch'` | **記憶體/臨時檔**：開啟一個無名暫存檔，**程式關閉後自動刪除**。 |
| | `'unknown'` | **預設值**：行為依編譯器而定（通常等同於存在則開啟，不存在則建立）。 |
| **`action`** | `'read'` | **唯讀**：禁止 `write` 操作，保護原始數據不被修改。 |
| | `'write'` | **唯寫**：僅允許寫入。 |
| | `'readwrite'` | **預設值**：允許同時讀取與寫入。 |
| **`position`**| `'asis'` | **預設值**：保持目前的檔案位置（通常是開頭）。 |
| | `'rewind'` | **強制歸零**：開啟檔案後將游標移至檔案最開頭。 |
| | `'append'` | **續寫模式**：將游標移至檔案末尾，新數據會接在舊數據後面。 |
| **`form`** | `'formatted'` | **文字檔 (ASCII)**：人類可讀，預設值。 |
| | `'unformatted'`| **二進位檔 (Binary)**：機器讀取，速度快、無精度損失、體積小。 |
| **`access`** | `'sequential'`| **順序存取**：由頭到尾依序讀寫（預設）。 |
| | `'direct'` | **直接存取**：需指定 `recl` (紀錄長度)，可隨機跳轉讀寫特定行。 |
| | `'stream'` | **流式存取 (F2003)**：最彈性的模式，將檔案視為連續位元組流（Binary 常用）。 |

##### 2. 常用參數組合範例

- **情境 A：安全讀取歷史數據**
    ```fortran
    open(unit=10, file='history.dat', status='old', action='read', iostat=ios)
    ```
- **情境 B：日誌記錄（Log）續寫**
    ```fortran
    open(unit=20, file='log.txt', status='unknown', position='append')
    ```
- **情境 C：高效能二進位數據儲存**
    ```fortran
    open(unit=30, file='raw_data.bin', form='unformatted', access='stream')
    ```

##### 3. 注意事項與禁忌
- **`status='scratch'` 限制**：使用暫存檔模式時，**不可指定 `file='檔名'`**，因為它是無名檔案。
- **`recl` (Record Length)**：當使用 `access='direct'` 時，必須提供 `recl` 參數，且單位在不同編譯器間（如 Intel vs GNU）可能代表「字元數」或「4位元組數」，需額外留意。
- **格式衝突**：若 `form='unformatted'`，則不可使用 `print` 樣式的格式化 `write` 指令。

##### 專業小撇步
在處理科學運算的大型矩陣時，建議優先考慮 `form='unformatted', access='stream'`。這種組合能避開文字轉換（如將 `3.1415926` 轉為字串）帶來的效能損耗，並保證浮點數在硬碟裡的每一位元都與記憶體中完全一致。

#### 4-3-3. 錯誤處理：使用 iostat 捕捉狀態

在處理外部檔案時，最常遇到的問題包括「檔案找不到」、「權限不足」或「硬碟空間已滿」。使用 `iostat` 參數可以捕捉這些執行時錯誤，讓程式優雅地處理例外，而不是直接崩潰。

##### 1. iostat 狀態碼解析
當你在 `open`、`read` 或 `write` 中加入 `iostat=ios_var` 時，編譯器會在操作後將結果存入該變數：

| `iostat` 數值 | 意義 | 建議處理動作 |
| :--- | :--- | :--- |
| **`0`** | **成功** | 繼續執行後續邏輯。 |
| **`> 0`** | **發生錯誤** | 檢查檔案路徑、權限、磁碟空間或資料格式。 |
| **`< 0`** | **檔案結尾 (EOF)** | 常用於 `read` 循環，代表資料已全部讀取完畢。 |

##### 2. 專業防錯架構範例
將你的程式碼擴充，加入 `iomsg` (Fortran 2003) 以顯示具體的系統錯誤訊息：

```fortran
program file_check
    implicit none
    integer :: u, ios
    character(len=100) :: msg  ! 用於儲存具體錯誤描述

    ! 嘗試開啟檔案
    open(newunit=u, file='input.dat', status='old', action='read', &
         iostat=ios, iomsg=msg)

    if (ios /= 0) then
        print *, "--- 檔案開啟失敗 ---"
        print *, "錯誤代碼 (iostat) ：", ios
        print *, "錯誤原因 (iomsg)  ：", trim(msg)
        print *, "--------------------"
        stop "程式終止：請確認 input.dat 是否存在於正確目錄。"
    else
        print *, "檔案開啟成功，單位編號：", u
        ! 開始讀取數據...
    end if

    close(u)
end program file_check

```

##### 3. 進階應用：在讀取循環中使用 iostat
在處理未知行數的數據檔時，這是最標準的寫法：

```fortran
do
    read(u, *, iostat=ios) val
    if (ios < 0) then
        print *, "讀取完成：已抵達檔案結尾。"
        exit
    else if (ios > 0) then
        print *, "讀取錯誤：資料格式不符！"
        exit
    end if
    ! 正常處理數據...
end do

```

##### 專業小撇步

- **平台差異**：雖然 `0` 代表成功是通用的，但不同編譯器（如 Intel Fortran 與 GNU gfortran）對於正值錯誤代碼（如檔案不見可能是 2 或是 29）的定義不同。因此，建議判斷時使用 `if (ios /= 0)`，而非特定數值。
- **防止讀取陷阱**：如果在 `read` 時沒有檢查 `iostat`，當讀到檔案末尾卻繼續讀取時，程式會發生崩潰。

#### 4-3-4. Inquire：檔案狀態查詢

`inquire` 指令允許程式在執行期間檢查檔案或單位編號（Unit）的屬性。這在處理自動化腳本或需要確認環境狀態時非常有用。

##### 1. 語法公式
你可以透過「檔案名稱」或「單位編號」來進行查詢：
```fortran
! 方式 A：透過檔名查詢 (檔案不一定要開啟)
inquire(file='檔名', [options])

! 方式 B：透過單位編號查詢 (通常用於檢查已開啟的檔案)
inquire(unit=編號, [options])

```

##### 2. 常用查詢參數速查表
所有的查詢結果都必須存入對應型別的變數中。

| 參數 | 回傳型別 | 說明 |
| --- | --- | --- |
| exist | logical | 檔案是否存在？（最常用） |
| opened | logical | 該檔案或編號目前是否處於開啟狀態？ |
| number | integer | 給定檔名，查詢其目前分配到的 Unit 編號（若未開啟則回傳 -1）。 |
| size | integer | 檔案的大小（單位：Bytes）。若檔案不存在則回傳 -1。 |
| action | character | 查詢權限：回傳 'READ', 'WRITE' 或 'READWRITE'。 |
| form | character | 查詢格式：回傳 'FORMATTED' (文字) 或 'UNFORMATTED' (二進位)。 |
| named | logical | 檔案是否有名字？（scratch 暫存檔會回傳 .false.）。 |

##### 3. 實戰範例：智能檔案檢查器
這是一個綜合應用，先檢查是否存在，再檢查大小，最後決定如何開啟。

```fortran
logical :: alive, is_open
integer :: f_size
character(len=20) :: access_type

inquire(file='data.bin', exist=alive, opened=is_open, size=f_size)

if (.not. alive) then
    print *, "錯誤：找不到 data.bin，無法繼續運算。"
else
    print *, "檔案狀態：存在"
    print *, "檔案大小：", f_size, " Bytes"
    
    if (is_open) then
        inquire(file='data.bin', action=access_type)
        print *, "警告：檔案已被開啟，權限為：", trim(access_type)
    end if
end if

```

##### 4. 專業小撇步

- **安全刪除**：在執行 `open(..., status='new')` 之前，先用 `inquire` 檢查 `exist`。如果檔案已存在，可以先提示使用者或自動備份，避免數據被覆蓋。
- **動態 Unit 管理**：如果你不確定某個編號（如 Unit 10）是否已被佔用，可以用 `inquire(unit=10, opened=is_open)` 來判斷，確保程式不會因為 Unit 衝突而報錯。
- **iostat 搭配**：`inquire` 本身也支援 `iostat` 參數，用來確認「查詢動作」本身是否成功（例如磁碟損毀可能導致查詢失敗）。

#### 4-3-5. 專業實務建議與隱藏機制

在大型科學計算專案中，正確管理檔案不僅能防止資料遺失，還能顯著提升 I/O 效能。

##### 1. 核心建議補全
- **Flush 指令（緩衝區刷新）**：
    - **原理**：寫入指令 `write` 通常只是將數據送到記憶體的「緩衝區」，而非立即寫入硬碟。
    - **語法**：`flush(unit)`。
    - **應用**：在長時間運算中，若不想關閉檔案但想確保數據已存入硬碟（以防斷電或當機），請定期執行此指令。
- **單位編號釋放**：
    - **原則**：始終記得 `close(u)`。當檔案關閉後，編號 `u` 會變回可用狀態。養成這習慣能避免在處理大量檔案時達到系統的「最大開啟檔案數」限制。
- **二進位 (Unformatted) 優勢**：
    - **效能**：處理數百萬筆複數座標時，使用 `form='unformatted'` 跳過了「數值轉字串」的耗時步驟，速度快 5-10 倍。
    - **精度**：二進位儲存保證了 100% 的浮點數還原，完全不會有 ASCII 轉換帶來的尾數捨入誤差。

##### 2. 你不可不知的「重要隱藏機制」 (補充建議)

除了上述幾點，以下是專業 Fortran 工程師在處理檔案時會特別注意的細節：

- **A. 非同步 I/O (Asynchronous I/O)**：
    - 如果你的計算量極大，可以使用 `open(..., asynchronous='yes')`。這允許程式在「背景」寫入檔案的同時，繼續進行下一行計算，實現計算與 I/O 並行。
- **B. 檔案鎖定 (File Locking)**：
    - Fortran 標準本身不提供檔案鎖，但如果你同時開啟多個程序讀寫同一個檔案，容易導致毀損。建議配合 `inquire(..., opened=is_open)` 檢查，或在 `open` 時使用 `action='read'` 來確保讀取安全。
- **C. 內部檔案格式 (Stream I/O)**：
    - 傳統的二進位檔案會包含「紀錄標記 (Record Markers)」，導致 C 語言或其他語言難以讀取。若要跟 Python/C 溝通，建議使用 `access='stream'`，這會產生最純粹的二進位流。
- **D. 暫存檔案的自動清理**：
    - 使用 `status='scratch'` 開啟的檔案，在 `close` 或程式異常終止時會被作業系統自動清除。這非常適合儲存運算過程中產生的巨大「中間矩陣」。

##### 3. 實務防錯檢查清單

| 檢查項 | 為什麼重要？ | 建議作法 |
| :--- | :--- | :--- |
| **路徑字串長度** | 檔名過長可能導致截斷。 | 宣告字串時給予足夠長度 `character(len=256)`。 |
| **殘留編號** | 重複開啟已佔用的 Unit 會噴錯。 | 優先使用 `newunit=u` 語法。 |
| **寫入權限** | 在唯讀目錄下執行 `open` 會失敗。 | 搭配 `iostat` 檢查並給予使用者友善提示。 |

#### 4-3-6. 檔案操作流程圖 (I/O Lifecycle)
在 Fortran 中，一個完整的檔案處理生命週期可以拆解為 **「偵測、連結、執行、清理」** 四個階段：

##### 1. 生命週期各階段詳解

1. **偵測階段 (Observation)**
  - **指令**：`inquire`
  - **目的**：在不開啟檔案的情況下確認環境（是否存在？容量多大？）。
  - **決策**：如果 `exist=.false.`，則決定報錯終止或跳過讀取。
2. **連結階段 (Connection)**
  - **指令**：`open`
  - **動作**：向作業系統申請一個 **Unit 編號** 與實體檔案路徑連結。
  - **參數影響**：`status` 決定是開啟舊檔還是建立新檔；`action` 決定讀寫權限。
3. **執行階段 (Execution)**
  - **指令**：`read` / `write`
  - **行為**：資料在記憶體變數與硬碟緩衝區之間傳輸。
  - **控制**：使用 `iostat` 監控是否讀到結尾（EOF）或發生硬體錯誤。
  - **即時同步**：若有極重要數據，執行 `flush` 強制將緩衝區寫入硬碟。
4. **清理階段 (Cleanup)**
  - **指令**：`close`
  - **動作**：解除 Unit 與檔案的連結，釋放系統資源。
  - **後置**：若為 `status='scratch'` 檔案，此時會從硬碟中徹底消失。

##### 2. 實戰流程範例：數據生產線
這段程式碼展示了從「偵測」到「關閉」的完整邏輯：

```fortran
logical :: already_exists
integer :: u, ios

! [1] 偵測：檔案是否在那裡？
inquire(file='data.raw', exist=already_exists)

if (already_exists) then
    ! [2] 連結：開啟舊檔讀取
    open(newunit=u, file='data.raw', status='old', action='read', iostat=ios)
    
    if (ios == 0) then
        ! [3] 執行：讀取數據
        read(u, *) my_data
        print *, "數據讀取成功！"
        
        ! [4] 清理：關閉連結
        close(u)
    end if
else
    print *, "錯誤：檔案遺失。"
end if

```

### 4-4. 進階數據交換 (Advanced I/O)

Namelist：最強大的參數檔讀取技術（像 JSON 一樣賦值）

Internal I/O：字串與數值間的快速轉換（字串做緩衝區）

Unformatted / Stream：巨量數據的二進位高速讀寫（無損失存儲）

## 5. 結構化與封裝 (Modularization)

### 5-1. 程式結構 (Structure)

#### Module：封裝變數與函數的標準做法  
#### Subroutine / Function：程序化開發  
#### Block Construct：臨時區域變數定義  
`block` 構造（Block Construct）最早引入於 **Fortran 2008** 標準。它允許在程式執行的過程中，開闢一個獨立的**局部作用域（Scope）**。  
1. 基本語法模板
    ```fortran
    [block_name:] block
        ! 1. 局部變數宣告 (Local Declarations)
        ! 2. 執行語句 (Executable Statements)
    end block [block_name]
    ```
    注意： 
    - 變數必須在 `block` 的最開頭宣告，就像在 `program` 或 `subroutine` 中一樣。  
    - 如果你在開頭寫了名稱，雖然結尾不一定要寫，但如果寫了（例如 `end block output`），則該名稱必須與開頭一致。這對於閱讀多層嵌套（Nested）的程式碼非常有幫助。

2. 核心特性
- **局部性 (Locality)**：在 `block` 內部宣告的變數，其生命週期僅限於該區塊。
- **變數遮蔽 (Shadowing)**：如果 `block` 內部宣告了與外部同名的變數，內部的變數會暫時「遮蔽」外部變數，且兩者互不干擾。
- **自動銷毀**：對於 `allocatable`（動態配置）的陣列，當程式離開 `block` 時，編譯器會**自動釋放其記憶體**，防止記憶體洩漏。

3. 三大應用場景  
    **A. 臨時變數的隨用隨宣告**  
    在舊式 Fortran 中，所有變數必須擠在程式最上方。`block` 讓你可以根據邏輯需要，在任何地方插入宣告。   
        
    ```fortran  
    do i = 1, 100
        ! ... 一些計算 ...
        output: block
            real :: temp_res  ! 只為了下面兩行計算存在的臨時變數
            temp_res = sqrt(real(i))
            print *, temp_res
        end block output
    end do
    ```
    
    **B. 記憶體管理（搭配 Allocatable）**  
    這是最實用的技巧。如果你需要一個巨大的暫存陣列，只需在使用它的地方開啟 `block`：  
    ```fortran
        block
            real, allocatable :: workspace(:,:)
            allocate(workspace(1000, 1000))
            ! ... 進行矩陣運算 ...
        end block ! <--- 離開後 workspace 自動 deallocate，釋放記憶體
    ```   
    
    **C. 避免邏輯衝突（命名空間保護）**  
    當你整合多人的程式碼或進行複雜實驗時，可以使用 `block` 隔離邏輯，不必擔心改動到全域變數。

4. 使用注意事項
    1. **效能考量**：在極高頻率的迴圈內部反覆進入 `block` 並配置大型 `allocatable` 陣列，可能會因頻繁申請記憶體而降低效能。  
    2. **跳轉限制**：你不可以從 `block` 外部直接用 `goto` 跳進內部；但可以用 `exit` 跳出一個具名的 `block`。  

#### Interface：顯示介面定義（解決參數傳遞疑難）

### 5-2. 現代進階特性 (Advanced)

#### Derived Types：自訂資料結構 (type)  
#### ISO_C_BINDING：與 C 語言/庫對接速查  
#### Intrinsic Modules：iso_fortran_env 精度標準化