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

#### 1. 陣列建構子 (Array Constructors)
這是在程式執行過程中，直接在程式碼裡「手寫」出一組數值的語法。

- **舊式語法 `(/ ... /)`**：相容性最強，所有版本皆支援。
- **新式語法 `[ ... ]`**：(Fortran 2003+) 語法更接近現代主流語言（如 Python、C#）。

**範例運用：**
```fortran
real :: vec(3)
vec = [1.0, 2.0, 3.0]                ! 使用中括號賦值
vec = (/ 1.0, 2.0, 3.0 /)            ! 使用舊式括號賦值（效果完全相同）

```

#### 2. 隱含式 Do 迴圈 (Implied-DO Loop)
這是 Fortran 的一大特色，讓你在宣告或賦值時，能用極短的程式碼生成規律的數值序列。

- **語法公式**：`[ (運算式, 變數 = 起始, 結束, 增量) ]`
**範例運用：**

```fortran
integer :: odds(5)
odds = [ (i, i = 1, 9, 2) ]          ! 生成 [1, 3, 5, 7, 9]

real :: squares(10)
squares = [ (real(i)**2, i = 1, 10) ] ! 生成 1 到 10 的平方數

```

#### 3. 單純陣列宣告 (不帶屬性)
最基本的陣列宣告法，其大小在編譯時就必須確定。

```fortran
real :: positions(3)                ! 一個長度為 3 的一維實數陣列
complex :: matrix(2, 2)             ! 一個 2x2 的二維複數陣列

```

#### 4. 基礎賦值與初始化

- **全體賦值**：`arr = 0.0` (將陣列中所有元素同時設為 0.0)
- **部分切片 (Slicing)**：`arr(1:3) = 1.0` (僅將前三個元素設為 1.0)

#### 5. 陣列存取與切片輸出 (Slicing)

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

#### 6. 注意事項

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

##### parameter：編譯期常數（安全性與效能）  
- 一個固定的數值，且不想讓任何人改動它時。   
- 如果程式後面嘗試修改，則編譯器會報錯攔截。  
-   ```fortran
    real, parameter :: pi = 3.14159265
    real, parameter :: c = 299792458.0
    real, parameter :: G = 9.81               ! 重力加速度
    integer, parameter :: MAX_USERS = 50      ! 最大使用者人數
    complex, parameter :: I = (0.0, 1.0)      ! 定義虛數單位 i
    ```

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

#### Logical IF：單行極速判斷  
時機：只執行一件小事  
`if (temperature > 100.0) print *, "警告：水分已沸騰！"`  
不需要 then 也不需要 end if  

#### Block IF：多行結構  
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


#### Select Case：多路分支與範圍匹配 (e.g., case (1:10))

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

---

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
這是現代 Fortran 的強大工具，它告訴編譯器：**每次循環都是獨立的**。
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

#### 💡 專業建議
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
  
  ! 💡 陣列威力：同時旋轉多個點
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

#### 1. 陣列切片進階運用 (Slicing)
透過冒號 `:` 語法，可以精準控制資料子集的提取與賦值。
- **語法公式**：`陣列名(起始索引 : 結束索引 : 步進間隔)`
- **範例應用**：
    ```fortran
    real :: A(10) = [ (real(i), i=1, 10) ]
    
    A(1:5) = 0.0             ! 前 5 個元素歸零
    A(::2) = A(::2) * 2.0    ! 所有「奇數位置」的元素翻倍
    A(10:1:-1)               ! 陣列反轉輸出
    ```

#### 2. 全陣列運算 (Whole-Array Operations)
只要陣列形狀（Shape）相同，即可直接進行算術運算。
```fortran
! 假設 A, B 是相同大小的陣列
C = A + B       ! 對應元素相加
D = A * B       ! 對應元素相乘 (注意：非矩陣乘法)
E = A > 0.5     ! 返回相同大小的邏輯陣列

```

#### 3. 條件遮罩運算 (Masking: WHERE Construct)
當你需要對陣列中「符合特定條件」的部分進行操作時，使用 `where` 語法：

```fortran
! 處理物理模擬中的邊界或異常值
where (data < 0.0)
    data = 0.0         ! 將所有負值強制歸零
elsewhere (data > 100.0)
    data = 100.0       ! 將過大值設為上限
end where

```

#### 4. 陣列內建函數速查表
以下函數皆支援「全陣列」輸入，是科學計算最常用的工具。

1.  屬性查詢
    | 函數名 | 功能說明 | 常用範例 |
    | --- | --- | --- |
    | size(arr) | 取得陣列元素總數 | n = size(A) |
    | shape(arr) | 取得各維度大小（回傳一維陣列） | dims = shape(matrix) |

2. 幾何重塑
    | 函數名 | 功能說明 | 常用範例 |
    | --- | --- | --- |
    | reshape(arr, shape) | 改變陣列維度（如 1D 轉 2D） | B = reshape(A, [2, 5]) |
    | transpose(mat) | 矩陣轉置（行列互換） | B = transpose(A) |

3. 數據歸約
    | 函數名 | 功能說明 | 常用範例 |
    | --- | --- | --- |
    | sum(arr) | 計算陣列所有元素總和 | total = sum(A) |
    | maxval(arr) | 找出陣列中的最大值 | peak = maxval(A) |
    | minval(arr) | 找出陣列中的最小值 | floor = minval(A) |
    | any(mask) | 只要有一個元素為真，即回傳 .true. | if (any(A<0)) stop |
    | all(mask) | 所有元素皆為真，才回傳 .true. | if (all(A>0)) ... |

4. 線性代數
    | 函數名 | 功能說明 | 常用範例 |
    | --- | --- | --- |
    | matmul(A, B) | 矩陣乘法（數學上的 A×B） | C = matmul(A, B) |
    | dot_product(V1, V2) | 兩向量的內積 | res = dot_product(u, v) |

#### 5. 專業小撇步：向量化優勢
在 Fortran 中，使用 `A = B + C` 而非 `do` 迴圈有兩個好處：

1. **程式碼簡潔**：大幅降低閱讀負擔，語法更接近數學公式。
2. **編譯器優化**：現代編譯器能自動針對這類語法進行 SIMD（單指令多數據）指令集優化，效能通常優於手寫迴圈。


## 4. 數據交換 (I/O & Files)

### 4-1. 標準輸入與輸出 (Standard I/O)
#### 1. 輸出：print 與 write
- print 語法：
    ```fortran
    print *, "這是一個字串", 123, 45.6
    ```
    星號 * 代表「使用預設格式」
- write 語法：
    比 print 更強大
    可以指定輸出的位置（如檔案）
    ```fortran
    write(*,*) "這是用 write 印出的內容"
    ! 第一個星號代表螢幕，第二個代表預設格式
    ```

#### 2. 輸入：read
- read 語法：  
    ```fortran
    integer :: age
    print *, "請輸入你的年齡："
    read *, age
    print *, "原來你 ", age, " 歲啊！"
    ```

#### 整合應用
假設我們要寫一個小程式，請使用者輸入圓形的半徑，然後計算並印出面積。
```fortran
program circle_calculator
    implicit none
    
    ! 1. 宣告常數與變數
    real, parameter :: PI = 3.14159265
    real :: single_radius, single_area
    
    ! 2. 宣告陣列：一次處理三個不同的半徑
    real, dimension(3) :: multi_radii
    real, dimension(3) :: multi_areas
    
    ! --- 單一數值處理 ---
    print *, "=== 單一圓形計算 ==="
    print *, "請輸入圓形的半徑："
    read *, single_radius
    
    single_area = PI * (single_radius**2)
    print *, "單一圓形的面積為：", single_area
    print *, ""
    
    ! --- 陣列（批量）處理 ---
    print *, "=== 批量陣列計算 ==="
    ! 給予陣列初始值
    multi_radii = (/ 1.0, 2.0, 5.0 /)
    
    ! 💡 重點：Fortran 不需要迴圈就能一次計算整個陣列！
    multi_areas = PI * (multi_radii**2)
    
    print *, "半徑列表：", multi_radii
    print *, "對應面積：", multi_areas
  
end program circle_calculator
```

### 4-2.  格式化控制 (Format Control)

基礎格式碼：整數 I, 實數 F, 指數 E, 字串 A

排版技巧：跳格 X, 定位 T, 換行 /

隱含 Do 迴圈：在一行內寫入整個陣列 (A(i), i=1, n)

### 4-3. 外部檔案操作 (File Management)

Open & Close：檔案編號 (Unit) 的選取與衝突避免

讀取模式：status, action, position 參數速查

錯誤處理：使用 iostat 捕捉檔案不存在或讀取中斷

Inquire：在程式運行中查詢檔案狀態（是否存在、大小等）

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
