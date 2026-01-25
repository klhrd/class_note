# FORTRAN 零基礎

## 目錄

### 第一篇：FORTRAN 零基礎入門
- **第 1 章：章一：為何學習 FORTRAN？基本概念與應用場景**
- **第 2 章：章二：搭建開發環境與第一個程式**
- **第 3 章：章三：基本語法與資料型態概覽**
### 第二篇：FORTRAN 基礎語法與流程控制
- **第 1 章：章四：流程控制：條件與迴圈**
- **第 2 章：章五：資料型態、變數與輸入輸出**
- **第 3 章：章六：陣列、子程式與模組**
### 第三篇：實作與最佳實踐
- **第 1 章：章七：小型科學計算案例：向量與矩陣運算**
- **第 2 章：章八：檔案 I/O 與資料管理**
- **第 3 章：章九：實務專案與除錯技巧**

---

# 第 1 篇：第一篇：FORTRAN 零基礎入門

## 第 1 章：章一：為何學習 FORTRAN？基本概念與應用場景

### 節1：什麼是 FORTRAN 與它的定位

FORTRAN，Formula Translation 的縮寫，是全球最早的高階語言之一，誕生於上世紀五十年代。它在科學與工程的數值計算領域長期佔有舉足輕重的地位，成為許多巨型計算程式與數值模型的基礎。雖然今天有許多新語言出現，但 FORTRAN 仍以穩定、高效與可移植著稱，尤其在巨型矩陣運算、稠密與稀疏數值解法、以及並行化優化方面保持競爭力。

定位上，FORTRAN 的核心是高效的數值計算與陣列処理。它天然支援多維陣列、向量化運算與內建的數值函式，讓科學計算程式的表達更接近數學描述。從學術研究到工業模擬，FORTRAN 常被用於天氣預報、量子化學、流體力學、結構分析、地球科學等領域。現代的 FORTRAN 2003/2008/2018 標準更強化了與其他語言的互操作性與物件導向特性，使它能與 C、Python 等語言共同工作，形成現代高效計算的生態圈。

在語言層面，FORTRAN 的優勢主要體現在以下幾點：
- 陣列與向量化：原生支援多維陣列，能以簡潔語法表達矩陣與向量運算，常見的操作如元素級加法、矩陣乘法等都可以高效完成。常見的時間複雜度分析，如向量加總為 $O(n)$，矩陣乘法在一般實作下接近或達到 $O(n^3)$ 的量級，這些都直接影響程式的性能走向。
- 精度與型別管理：使用 kind 機制來控制浮點與整數的精度，實作可預測的數值行為，並能在不同平台保持一致性。單精度與雙精度的概格在 IEEE 754 標準下通常分別近似為 $24$ 位有效位元與 $53$ 位有效位元。
- 跨平台與可移植性：FORTRAN 程式可在多種作業系統與編譯器上編譯執行，經由標準化語法降低平台差異所帶來的風險。
- 與其他語言的互操作性：透過 ISO_C_BINDING，FORTRAN 程式可以與 C、C++ 等語言互相呼叫，方便整合現有的科學計算套件。

值得注意的是，FORTRAN 的預設陣列下界是 1，這與很多語言不同；你也可以在宣告中指定自訂的下界，例如 real(kind=8), dimension(-5:5) :: v。這種特性雖增添彈性，但在團隊協作與閱讀性上也需要一致的規範。若你以單純的教學角度入門，先把 1 為起點的用法熟悉，再逐步了解下界可調整的情境，會比較穩妥。

現今的 FORTRAN 編譯器眾多，常見的如 gfortran、Intel Fortran (ifort)、NAG、PGI 等。這些編譯器提供優化選項、向量化支持與並行化工具，能在現代高性能計算平台上發揮極致效能。為了與其他語言協作，學習者應留意編譯器版本、標準支援程度以及與外部函式庫的介接方式。

下方提供兩個簡單的程式範例，幫助你感受 FORTRAN 的語法風格與編譯流程的直覺感受：
```fortran
program hello
  implicit none
  print *, "Hello, FORTRAN!"
end program hello
```

```fortran
program array_sum
  implicit none
  integer, parameter :: n = 5
  integer, dimension(n) :: a = (/ 1, 2, 3, 4, 5 /)
  integer :: i, sum
  sum = 0
  do i = 1, n
     sum = sum + a(i)
  end do
  print *, "Sum =", sum
end program array_sum
```

這些範例展現了資料型別、控制結構與輸出方式的基本用法，亦能幫助你理解編譯與執行的基本流程。當你開始撰寫更複雜的數值模型時，FORTRAN 的語法會逐步呈現出它對數值穩定性與可讀性的重視。

為了建立後續章節的鋪陳，下一節將帶你實作第一個實用的小練習，並介紹基本編譯器選項與除錯流程，以降低初學者在入門初期的阻力。接著，我們會逐步引入陣列運算、子程式與模組等重要概念，讓你在實作中理解 FORTRAN 的定位與優勢。

### 節2：FORTRAN 的特色與常見用途

FORTRAN 作為最早為數值計算而生的高階程式語言，至今在科學與工程領域仍具有不可替代的地位。它的設計重點放在數值穩定性、可預測的執行效能，以及對陣列運算的天然支援。以下重點整理適合零基礎到進階的學習者，幫助你理解為什麼要選擇 FORTRAN，以及在實務中該怎麼運用它。

- 數值計算的核心長處
  - 浮點運算的穩定性與可預測性：FORTRAN 對浮點數有穩定的語意與廣泛的數值函式支援，適合解線性方程組、微分方程、優化問題等。機器精度的概念常以 $\epsilon_{\text{mach}} \approx 2^{-52} \approx 2.22 \times 10^{-16}$（對於 IEEE 754 雙精度）表示。相關誤差常以公式表示：  
  $$\frac{|x_{\text{true}}-x_{\text{approx}}|}{|x_{\text{true}}|} \le \kappa(A)\,\epsilon_{\text{mach}}$$  
  ，其中 $\kappa(A)$  為條件數，提醒你數值問題的難度往往來自問題本身的性質。

- 陣列運算與向量化的天然支援
  - FORTRAN 對陣列的運算支援很自然，能以元素級寫法表達複雜的計算，常見的操作如向量加法、逐元素乘法、以及內建的 dot_product、sum、product 等函式，讓程式碼更簡潔、易讀。
  - 陣列的記憶體佈局通常相當緊湊，利於編譯器做自動向量化與最佳化，適合大規模數值模擬。

- 模組化與可維護性
  - 從 Fortran 90/95 開始引入模組（module）、介面（interface）與泛函式（generic procedures），讓程式碼可分解、重複使用，並提升大型專案的維護性。你可以把相同功能的子程式放在模組中，對外只暴露必要介面，降低耦合度。

- 與現代工具的互操作性
  - 現代 FORTRAN 支援與 C、Python 等語言的互操作，透過 ISO_C_BINDING 等機制，便於在多語言環境中整合。常見的工作流程是用 FORTRAN 做數值核心，外部介面再用其他語言做介面與資料前處理/後處理。許多高效的線性代數庫（如 BLAS/LAPACK）也能直接在 FORTRAN 程式中呼叫，取得穩定且經過優化的性能。

- 銜接現場問題的語法與可讀性
  - Fortran 的語法在數值計算領域被長期磨鍊，對變數型別、界線檢查、迴圈行為等有清楚的規範，能降低由語意不清引起的錯誤。現代版本在可讀性與嚴謹性上有顯著提升，適合新手逐步養成良好的編程風格。

- 常見使用場景
  - 科學與工程模擬：氣象、天體力學、流體動力學、計算化學、材料科學等。 
  - 大型線性代數與特徵值問題：需要穩定的數值求解與高效的矩陣運算時，FORTRAN 常是首選或核心部分。
  - 與高效函式庫結合：直接呼叫 BLAS/LAPACK、或在模組化結構中整合自訂工具，方便長期維護與再利用。

- 實作層面的小提示
  - 初學者在寫作時可先以固定格式或自由格式的語法為練習起點，逐步體會模組化與介面設計的好處；在追求效能時，再學習編譯器選項（如優化、除錯檢查）與跨平台的可攜性要點。

以下是一個簡單的範例，說明元素級運算與內建函式的基本用法，供你參考與實作練習：

```fortran
! 範例：基礎陣列運算與內建函式
program basic_ops
  implicit none
  integer, parameter :: n = 5
  real(kind=8), dimension(n) :: A, B, C

  A = (/ 1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0 /)
  B = (/ 1.0d0, -1.0d0, 0.5d0, 2.0d0, -0.5d0 /)
  C = A + B                     ! 元素級加法

  print *, "A =", A
  print *, "C = A + B =>", C
  print *, "dot_product(A,B) =", dot_product(A, B)
end program basic_ops
```

透過這些特色與實務案例，你可以理解為何 FORTRAN 在數值計算領域長青，以及該如何在學習初期就著手建立穩健與高效的程式設計習慣。下一節將帶你實作第一個實用的小練習，並介紹基本編譯器選項與除錯流程，以降低初學者在入門初期的阻力，並鋪陳後續的陣列運算、子程式與模組等概念。

### 節3：常見的編譯器與工具

在學習 FORTRAN 的起步階段，選擇一組穩定的編譯器和周邊工具，是培養穩健習慣的關鍵。不同作業系統與硬體環境下，常見的選項分為免費/商用兩大類，以下整理出實務上較常見且易於入手的組合。

- 常見編譯器家族
  - GNU Fortran (gfortran)：開源、跨平台，支援 Fortran 2008/2018 標準，適合初學者與研究型工作。常用組合是 -g（含除錯資訊）、-Wall/-Wextra（警告完整性）、-O2 或 -O3（優化），並配合 -fcheck=all、-fbacktrace 以提升除錯與穩定性。行業上也廣泛用於教學與學術專案。
  - Intel Fortran (ifort / ifx)：在數值運算與矢量化方面表現出色，適合需要極致效能的專案。常見旗標包括 -O3、-xHost、-qopenmp、-check all、-traceback 等；同時建議啟用 -assume realloc_shrink、-frecursing-... 之類選項以提升穩定性。若使用 Intel 環境，需安裝授權並設置環境變數。
  - NVIDIA HPC SDK 的 nvfortran / pgfortran：面向高效能計算與 MPI/OpenMP 的工作負載，對大型平行計算友善。適合需要 GPU 加速或跨平台叢集的開發場景。
  - 其他商用/學術選項：IBM XL Fortran、PGI/PGF / NVIDIA 的過往版本等。不同編譯器在語法容錯與向量化行為上會有差異，選用時宜以「穩定性 + 需求的最佳化」為主。

- 作業系統與 IDE 環境
  - Windows：可以搭配 Intel Fortran 與 Visual Studio，或在 VS Code/Code::Blocks 等編輯器中使用 fortls、Modern Fortran 等插件增強語法高亮與自動完成。
  - macOS / Linux：gfortran 是入門首選，配合 Makefile 或 CMake 就能快速起手。背景除錯可使用 gdb，OpenMP 則用 -fopenmp。
  - 常見 IDE/編輯器：Eclipse + Photran、Visual Studio Code（搭配 fortls、Fortran Language Server）、Code::Blocks 等。

- 建置與自動化
  - Makefile：對初學者最直覺的自動化方式，能建立編譯、連結、清除等常用規則，讓專案結構逐漸穩定。
  - CMake：跨平台建置系統，能自動產生各平台的專案設定檔，適合日後拓展到複雜的模組與多語言專案。
  - 版本控制：建議與 Git 結合，對於追蹤編譯選項與巨量修改十分有用。

- 除錯與效能分析
  - 除錯：編譯時加入 -g、-fcheck=all、-fbacktrace、-traceback，搭配 gdb 進行變數檢視與步進。
  - 計時與分析：編譯時使用 -pg（gprof）或 perf 等工具查看函式層級與熱點；對 MPI/OpenMP 程式，亦可使用專屬的分析工具如 Intel VTune、Valgrind 記憶體檢查等。
  - 設計例子：若以單檔測試，先用簡單的 dot_product 或矩陣乘法作為基準，逐步加入平行化與向量化觀察效能變化。

- 參考與實作範例
  以下給出一組常見工作流程的參考，方便你在本章節複習時實作與實驗。

Code Block: Makefile 範例
```make
# Makefile 示範：使用 gfortran
FC = gfortran
FCFLAGS = -Wall -Wextra -g -O2 -fcheck=all
LDFLAGS =

SRC = main.f90
EXE = main

$(EXE): $(SRC)
	$(FC) $(FCFLAGS) -o $(EXE) $(SRC) $(LDFLAGS)

.PHONY: clean
clean:
	rm -f $(EXE)
```

Code Block: 常用編譯指令
```bash
# 基本編譯與執行（gfortran）
gfortran -g -O2 -fcheck=all -Wall -Wextra -fimplicit-none main.f90 -o main
./main
```

Code Block: gdb 除錯入門
```bash
# 編譯時加入 -g，啟動 gdb
gfortran -g -O0 main.f90 -o main
gdb ./main
# 在 gdb 內基本操作
(gdb) break main
(gdb) run
(gdb) print x
(gdb) next
(gdb) continue
```

- 專案設計的評估公式（示意性，幫助思考取捨）：
  為了在不同編譯器與策略間做比較，我們可以用一個整體成本函數 $S$ 作為指標，其中
  $$
  S_{\text{total}} = w_c \, T_{\text{compile}} + w_r \, T_{\text{run}} + w_m \, M_{\text{memory}}
  $$
  其中 $T_{\text{compile}}$、$T_{\text{run}}$、$M_{\text{memory}}$ 分別對應編譯時間、執行時間與記憶體佔用，$w_c, w_r, w_m$ 為權重。透過這樣的權重方法，可以在不同專案需求下快速做取捨與實驗。

結語上，熟悉常見編譯器與工具不僅能降低入門阻力，更能幫助你在日後的陣列運算、子程式與模組開發中，專注於演算法與程式設計邏輯，而非工具的生疏。下一節將帶你實作第一個小練習，並示範如何建立自己的測試與除錯流程，讓你開始建立穩健的學習與開發節奏。在你掌握基本的編譯選項與檔案結構後，便能逐步深入到陣列運算、高階子程式與模組等概念。

### 節4：建立專案結構與工作目錄

在開始撰寫 Fortran 程式前，先建立一個清晰、可擴充的專案結構，使日後的編譯、測試與除錯都能高效進行。既然前文提到 $m$ 為權重，用於在不同專案需求下快速取捨與實驗，那麼我們就把整個專案拆解成多個模組與子任務，並以權重分配開發重心。若把每個模組的工作量用 $m_i$ 表示，總成本可用公式 
$$C_{\text{total}} = \sum_{i=1}^{n} m_i$$
表示，其中 $n$ 是模組數，$m_i$ 為第 $i$ 個模組的難度與工作量。透過這樣的計算，你可以在規劃初期就清楚哪些模組需要優先完成或重點測試。

一、建議的專案目錄結構
- src/：放置實際的程式來源 (.f90、.f95 等)。
- modules/ 或 include/：放置共用模組或 include 片段。
- build/：編譯過程中的中間檔與輸出檔放置區。
- bin/：最終可執行檔或測試執行檔。
- tests/：單元測試與整合測試案例。
- docs/：開發文檔與使用說明。
- .git/：版本控制資料夾（若使用 Git）。

二、實作範例 / 標準目錄樹
可參考以下樹狀佈局作為起點，日後再依需求微調：
```
ProjectRoot/
├── src/
│   ├── main.f90
│   └── calc.f90
├── modules/
│   └── util.f90
├── include/
├── build/
├── bin/
├── tests/
├── docs/
└── .gitignore
```

三、常用工具與檔案範例
- Makefile（以 gfortran 為例的簡易範本）
```makefile
# Makefile
FC := gfortran
SRC := $(wildcard src/*.f90) $(wildcard modules/*.f90)
OBJ := $(SRC:.f90=.o)
BUILD := build
BIN := bin/program

all: $(BIN)

$(BIN): $(OBJ)
	@mkdir -p $(BUILD) $(BIN)
	$(FC) -o $@ $(OBJ)

%.o: %.f90
	$(FC) -c $< -o $(BUILD)/$@

clean:
	rm -f $(BUILD)/*.o $(BIN)
```

- .gitignore（避免把編譯產物放進版本庫）
```gitignore
# Build outputs
build/
bin/

# Fortran objects and modules
*.o
*.mod
*.lst

# Editor
*.swp
*.swo
```

四、範例程式：模組與主程式
- 模組檔 src/modules/util.f90
```fortran
module util
  implicit none
contains
  subroutine show-welcome()
    implicit none
    print *, "Hello from Fortran module: util"
  end subroutine show_welcome
end module util
```

- 主程式 src/main.f90
```fortran
program main
  use util
  implicit none
  call show_welcome()
end program main
```

五、編譯與執行流程
- 建立目錄與預備工作
  - mkdir -p build bin
  - 將 src、modules 下的檔案準備就緒
- 編譯與連結
  - make（若使用上述 Makefile，直接執行 make 即可）
  - 或手動編譯：gfortran -c src/main.f90 -o build/main.o
  - gfortran -o bin/program build/main.o
- 執行
  - ./bin/program

六、工作流與測試規劃
- 建立測試目錄 tests/，撰寫簡單的測試案例，如輸出比對、函式回傳值等。
- 每次大型變更後，先跑通基本編譯與最小單元測試，再進行整合測試。
- 使用簡易除錯策略：開啟 Fortran 的運行時檢查，例如在編譯時加入 -fcheck=all、-Wall 等旗標，並在程式中加入必要的輸出以追蹤流程。

七、組織與協作的小貼士
- 對於每個模組設定合理的 m_i 權重，讓最核心的模組先穩定再逐步擴充。
- 遵循一致的命名規則與檔案分佈，避免未來難以追蹤的相依性問題。
- 使用版本控制與分支策略，將功能、修正與實驗分開管理。

結語上，熟悉常見編譯器與工具不僅能降低入門阻力，更能幫助你在日後的陣列運算、子程式與模組開發中，專注於演算法與程式設計邏輯，而非工具的生疏。下一節將帶你實作第一個小練習，並示範如何建立自己的測試與除錯流程，讓你開始建立穩健的學習與開發節奏。在你掌握基本的編譯選項與檔案結構後，便能逐步深入到陣列運算、高階子程式與模組等概念。

### 節5：第一個程式：Hello, World

在零基礎的起點，你最先要學會的是一份能正確輸出的最簡單程式。Hello, World 看似只有一行字，但它實證了整個開發流程：建立檔案、編譯、執行，以及程式與輸出之間的基本關係。為了避免日後的混亂，建議以版本控制管理專案，並以分支策略將功能、修正與實驗分開，例如 feature/hello-world、fix/typo、experiment/printf-format 等。這樣你在學習與實驗時就不會互相干擾，日後擴充也更易於追蹤與回溯。

先了解一些小概念再動手：
- Fortran 的字串輸出通常使用 print 句法，若未指定格式，預設會以寬鬆的方式顯示內容，如 print *, "Hello, World"。
- 為了讓編譯器幫你避免常見錯誤，建議在程式開頭加入 implicit none，強制所有變數必須顯式宣告。
- 關於整數與實數的運算，Fortran 的整數除法在正整數情況下商為 $Q = \\left\\lfloor \\dfrac{A}{B} \\right\\rfloor$，餘數可用 $R = A - B \\cdot Q$ 計算。區塊公式如下所示：  
  $$ 
  Q = \\left\\lfloor \\dfrac{A}{B} \\right\\rfloor, \quad
  R = A - BQ
  $$

以下是一個最基本的 Hello, World 程式，採用自由格式（.f90）並含有嚴格型別檢查的慣例：

```fortran
program hello
  implicit none
  ! 宣告完畢後，接著進入主流程
  print *, "Hello, World"
end program hello
```

逐行解說：
- program hello: 程式入口與識別名稱。
- implicit none: 禁止自動型別推斷，遇到未宣告的變數會錯誤提醒，適合新手避免暗藏 bug。
- print *, "Hello, World": 將字串輸出到螢幕，星號代表自動格式化。
- end program hello: 結束程式，括號內的名稱與開頭名稱一致。

如何編譯與執行是新手最常遇到的問題。以常見的開放原始碼編譯器為例：
- 使用 gfortran（Linux/macOS）：
  - 編譯：gfortran -Wall -Wextra -std=f95 hello.f90 -o hello
  - 執行：./hello
- 使用 Intel Fortran（ifort）：
  - 編譯：ifort -stand f95 -o hello hello.f90
  - 執行：./hello
- Windows（MinGW 與 gfortran）：
  - 編譯指令同上，執行時改用 hello.exe。

常見錯誤與解法：
- 路徑或檔名錯誤：確保檔案副檔名為 .f90，且執行時在正確工作目錄。
- 語法或大小寫問題：Fortran 不分大小寫，但命名區分大小寫在某些編譯器設定下可能產生差異，建議統一使用小寫。
- 沒有 implicit none 的風險：若未宣告變數，某些編譯器會自動推斷，容易導致難以追蹤的錯誤。

在版本控制方面的實務建議：
- 初始化倉庫：git init
- 新功能分支：git checkout -b feature/hello-world
- 新增檔案並提交：git add hello.f90；git commit -m "feat: add Hello, World example"
- 修正錯字或格式：git checkout main；git pull；git checkout fix/typo；git commit
- 合併回主幹：使用 pull request 或 git merge

結尾與展望：第一個程式帶給你的不只是輸出一句字，它同時讓你感受到專案結構、編譯流程、以及版本控制的基礎節奏。掌握這些後，下一節我們將加入使用者輸入與格式化輸出，使程式具有互動性，並開始建立簡單的測試與除錯流程，為後續的子程式、模組與陣列運算奠下穩固基礎。接著，你會在實作中逐步欣賞演算法設計與程式邏輯的分工，並培養能在工具與編譯選項間保持專注的學習節奏。

## 第 2 章：章二：搭建開發環境與第一個程式

### 節1：安裝編譯器（如 GNU Fortran）

Fortran 的編譯需要合適的編譯器。GNU Fortran（gfortran）是開源且跨平台的選擇，能支援 F90/95/2003/2008 等版本。正式動手前，先確定你的作業系統與安裝方式，並在終端機或指令提示字元中驗證版本。

- Windows 的安裝方式
  - 建議透過 MSYS2 或 MinGW-w64 的環境安裝。以 MSYS2 為例：
    - 安裝 MSYS2，打開「MSYS2 MinGW 64-bit」終端。
    - 更新系統：pacman -Syu；若出現關閉再開啟的提示，依指示重新啟動終端。
    - 安裝 gfortran：pacman -S mingw-w64-x86_64-gfortran。
    - 將 C:\msys64\mingw64\bin 放入 PATH，重新打開終端，驗證：gfortran --version。
  - 安裝成功後即可如後述編譯。

- macOS 的安裝方式
  - 使用 Homebrew 安裝 GCC（其中包含 gfortran）：brew install gcc。
  - 安裝完成後，檢查路徑與版本：gfortran --version。若系統預設路徑未更新，將 /usr/local/opt/gcc/bin 等加入 PATH。

- Linux 的安裝方式
  - Debian/Ubuntu：sudo apt update; sudo apt install gfortran
  - Fedora：sudo dnf install gcc-gfortran
  - Arch Linux：sudo pacman -S gcc-fortran
  - 安裝後再次執行 gfortran --version 以確認可用。

- 第一個程式與編譯
  - 新增檔案 hello.f90，內容如下：
  ```fortran
  ! hello.f90
  program hello
    print *, "Hello, Fortran!"
  end program hello
  ```
  - 編譯與執行：
  - 在終端機輸入：gfortran hello.f90 -o hello
  - 執行：./hello（Windows 為 hello.exe）
  - 預期輸出：Hello, Fortran!

- 常見問題與解法
  - 找不到 gfortran：確認 PATH 指向已安裝的 bin 資料夾；有時需重新啟動終端機。
  - 執行權限問題：在 UNIX 系統確保可執行檔有執行權限。
  - 版本與標準的選擇：可加入 -std=f2008 等選項來指定語言標準，例如 gfortran -std=f2008 hello.f90 -o hello。

- 開發工作流的實用建議
  - 將範例程式放入版本控制中，如 Git，逐步提交變動，與「安裝步驟」與「第一個程式」形成清晰的紀錄。
  - 為日後的編譯建立簡單的 Makefile，或在 VS Code、Sublime 等編輯器中設定自動化任務，以提升重複性工作效率。

- 小結與展望
  安裝與驗證完成後，你已建立基本的編譯環境與第一個可執行程式，接著可加入使用者輸入與格式化輸出的練習，並逐步建立測試與除錯的習慣。下一節我們將實作第一個具有互動性的版本，並探討輸入輸出與基本格式化的技巧。

### 節2：設定編譯與執行流程

在本節，我們將建立穩定的編譯與執行流程，讓未來的修改與測試更高效。重點包含：建立簡單的 Makefile、示範可重複使用的編譯指令、以及在常見編輯器中設定自動化任務。為了讓整體流程有可追蹤性，我們也會提到如何在除錯模式下編譯與執行。值得一提的是，編譯與連結的時間與程式大小之間，往往呈現近似的關係，可以用下列簡單模型表示：
$$T(S) = a + b \cdot S$$
，其中 T 為編譯時間、S 為程式碼行數，a 與 b 為常數。若以此觀點看待，編譯時間與程式規模具有線性趨勢，這也說明自動化任務的重要性：當你增加檔案數量時，能自動化整個流程而不是手動重複操作。又可寫成行內式：$T \propto S$，以強調隨著大小成正比的趨勢。

一、Makefile 的基礎搭建
以下提供一個最小可用的 Makefile，適用於單檔案的 .f90 或 .F90 程式。若專案檔案較多，只需要改寫 SRC 變數即可自動擴充。

```makefile
# Makefile
# 編譯器與旗標
FC = gfortran
FFLAGS = -Wall -Wextra -O2 -fimplicit-none

# 檔案清單與輸出
SRC = main.f90
OBJ = $(SRC:.f90=.o)
EXEC = main

.PHONY: all clean run

# 預設執行目標
all: $(EXEC)

# 連結成可執行檔
$(EXEC): $(OBJ)
	$(FC) $(FFLAGS) -o $@ $^

# 逐一編譯
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

# 執行
run: $(EXEC)
	./$(EXEC)

# 清除暫存與可執行檔
clean:
	rm -f $(OBJ) $(EXEC)
```

說明與注意事項
- 若使用 Windows 的 MinGW/MSYS2，執行檔名會變成 main.exe，此時 run 的指令要改成 .\$(EXEC).exe。
- 為避免語法錯誤，請確保每個編譯階段的命令必須用 Tab 總是替代空格縮排。

二、範例程式（main.f90）
為了讓讀者能立即實作與驗證，提供一個最簡單的範例。

```fortran
program main
  implicit none
  print *, "Hello, FORTRAN!"
end program main
```

三、在編輯器中設定自動化任務
- VS Code 的任務 (tasks.json) 範例

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "build-fortran",
      "type": "shell",
      "command": "make",
      "group": {
        "kind": "build",
        "isDefault": true
      },
      "problemMatcher": [
        "$gcc"
      ]
    },
    {
      "label": "run-fortran",
      "type": "shell",
      "command": "./main",
      "group": "test",
      "presentation": { "reveal": "always" }
    }
  ]
}
```
在 Windows 環境下，若執行檔為 main.exe，請將 run 的命令改成 ".\\main.exe"。

- Sublime Text 的 build system（Build 系統）範例

```json
{
  "cmd": ["bash", "-lc", "make && ./main"],
  "selector": "source.fortran",
  "path": "${project_path}"
}
```

四、在不同環境的執行與除錯要點
- Linux/macOS 常用指令：make、./main。若要加入偵錯資訊，可在編譯時加上 -g：FFLAGS = -g -Wall -Wextra。
- Windows + MinGW：使用 gfortran 同樣可用，若要用 gdb 進行除錯，需用 -g 產生符號檔，再執行 gdb ./main。
- 對於大型專案，建議把多個 .f90 檔案放入 src/，並在 Makefile 內以通用規則自動產生對應的 .o 與最終的執行檔。你也可以加入自動化單元測試或簡單的輸出比對，以提高穩定性。

五、在編輯器與版本控制中的好習慣
- 將 Makefile 與 sourc e code 一同放在版本控制中，避免環境差異造成編譯失敗。
- 設定版本控制前後的編譯環境說明（README），以及在 CI 內部跑過的編譯步驟，降低新同學加入的門檻。
- 如果你偏好文字編輯器的熱鍵與快速切換，請把「編譯」與「執行」設定成一鍵執行，讓每次修改後的檢查更快速。

六、額外的實務建議
- 對於想要更細緻的除錯，可以在程式中打點輸出，搭配 -fcheck=all 與 -fbacktrace，協助找出運算或記憶體相關的錯誤。
- 為避免編譯過程中的隱性問題，建議開啟 -Wall、-Wextra、-Wconversion 等旗標，並習慣先以 -O0 測試，再在穩定後切到 -O2。
- 如需同時編譯多檔，可改用 SRC = $(wildcard src/*.f90) 並略增一層目錄結構，讓專案更具可擴充性。

結尾過渡
至此，你已建立基本的編譯與執行流程，並能在常見編輯器中透過自動化任務快速完成編譯與執行。接下來我們將進入「具互動性的第一個版本」，練習使用者輸入與格式化輸出，並開始建立簡易測試與除錯的習慣，讓程式更穩健地與使用者互動。

### 節3：編譯常見錯誤與排解

本節聚焦 Fortran 零基礎在實作與編譯階段容易踩到的坑，提供清晰的排解步驟與實例。為了讓排解更具有效率，先把編譯與連結的流程做穩固，再逐步開啟額外檢查與偵錯工具。為了提升可預測性，建議開啟嚴格檢查與執行期檢查，並在初期使用 -O0 測試，穩定後再切到 -O2 以取得性能提升。常見的檢查要點可用下列縮寫記憶：語法、模組與介面、型別、陣列邊界，以及編譯順序與路徑。

- 編譯與警告的快速設定
  - 基本：gfortran -g -O0 -Wall -Wextra -o main src/*.f90
  - 開啟執行期檢查與邊界檢查：gfortran -g -O0 -fcheck=all -fbacktrace -fbounds-check -o main src/*.f90
  - 如需多檔編譯，先將檔案列出再連結：SRC = $(wildcard src/*.f90)；gfortran -g -O0 -o main $(SRC)

- 常見錯誤類型與排解要點
  - 語法與未宣告變數
  - 模組與介面不一致
  - 型別不匹配與介面
  - 陣列越界與運算過界
  - 編譯順序與連結順序

- 逐案示例與修正
 1) 未宣告變數（使用 implicit none）  
 不良寫法（未宣告變數，且啟用 implicit none 將無法通過）：
```fortran
program test
implicit none
x = 5
print *, x
end program test
```
 修正後：
```fortran
program test
implicit none
integer :: x
x = 5
print *, x
end program test
```

 1) 模組與用法的順序與介面
 不良寫法可能出現“Cannot find module”或“Error: Could not open module file”：
```fortran
! main.f90
program main
use modmesh
implicit none
call show
end program main
```
模組檔：
```fortran
! modmesh.f90
module modmesh
  implicit none
contains
  subroutine show
    print *, "Module works"
  end subroutine show
end module modmesh
```
正確的編譯順序與內容：
```bash
gfortran -c modmesh.f90
gfortran -c main.f90
gfortran -o prog main.o modmesh.o
```
或一次編譯也可：
```bash
gfortran modmesh.f90 main.f90 -o prog
```

 1) 陣列越界與執行期檢查
 不良寫法：
```fortran
program bounds
implicit none
integer, dimension(3) :: arr
arr(4) = 5
end program bounds
```
啟用邊界檢查的建議執行：
```bash
gfortran bounds.f90 -o bounds -fcheck=all -fbacktrace
```
正確寫法（避免越界）：
```fortran
program bounds
implicit none
integer, dimension(3) :: arr
arr(3) = 5
end program bounds
```

 1) 型別不匹配與介面問題
 不良寫法（子程式介面未正確宣告，呼叫端與子程式型別不一致）：
```fortran
! scale.f90
subroutine scale(x)
  integer :: x
  x = x * 2
end subroutine scale

! main.f90
program test
implicit none
integer :: a
call scale(a)
end program test
```
修正時，給予一致的介面與型別：
```fortran
! scale.f90
subroutine scale(x)
  real, intent(inout) :: x
  x = x * 2.0
end subroutine scale
```
```fortran
! main.f90
program test
implicit none
real :: a
a = 3.0
call scale(a)
print *, a
end program test
```

- 進階診斷與最佳實踐
  - 啟用顯式介面與模組化：使用 use 模組，確保子程式介面可被正確連結。
  - 啟用嚴格檢查：-fcheck=all、-fbounds-check、-fbacktrace 能在執行期即時回報錯誤來源與回溯。
  - 頻繁檢視錯誤訊息：閱讀錯誤訊息的檔案與行數，定位到對應的程式區段，並用小型測試範例逐步排除。
  - 測試輸入與輸出格式：先用固定的輸入檔案測試輸出格式，再逐步加入使用者互動。
  - 設計可重現的最小案例：把錯誤縮減為最小可重現程式，便於查找與他人討論。

- 小結與過渡
 透過上述排解流程與實例，你已具備在實際專案中快速定位與修正編譯與執行期錯誤的能力。接下來我們將進入「具互動性的第一個版本」，練習使用者輸入與格式化輸出，並開始建立簡易測試與除錯的習慣，讓程式更穩健地與使用者互動。

### 節4：程式檔案與檔案命名約定

本節專注於在專案初期就建立清晰、可重現的檔案命名與結構規範。對於 FORTRAN 零基礎的開發者，統一的命名習慣能降低編譯與除錯時的困惑，也方便後續的測試與版本控管。為跨平台作業，建議以可預期的規則來處理副檔名、大小寫與目錄層次，讓同樣的檔案命名在 Windows 與 Linux 上都能穩定運作。

- 副檔名與意義
  - 常見副檔名包括 .f、.for（固定列長）、.f90、.f95（自由格式）。其中 .F90、.F95 等大寫副檔名通常會觸發預處理器（CPP），需要在編譯時加上 -cpp；若不需要預處理，建議盡量使用小寫的 .f90、.f95。為避免混亂，建議全團隊統一使用小寫副檔名並避免混用大寫。
  - 內嵌公式：檔案長度與路徑長度通常有上限，如常見系統為 $N \le 255$ 字元，實務上仍應限制到 $<255$，以避免跨平台的檔案截斷或錯誤。

- 檔案名稱規格
  - 字元與分隔：僅使用小寫英文字母、數字與底線 _，避免空格與特殊符號。檔名長度以易於管理為原則，避免過長。
  - 結構命名：檔名應描述內容與模組範圍，例如 src/matrix_utils.f90、src/solver/fixed_point.f90。若有多個子模組，建議以底線區隔，保持可讀性與排序性。
  - 模組對應：若檔案內宣告了模組，建議檔名與模組名保持一致，或以一致的前綴命名，例如模組 matrix_utils 對應檔名 matrix_utils.f90。

- 目錄與專案結構
  - src/ 放來源檔案；include/ 放介面或公用介面檔； test/ 放測試用來源； bin/ 放編譯後執行檔； doc/ 放文件與說明。
  - 以穩健的結構利於自動化測試與版本控管，若專案成長，亦可分模組子目錄，如 src/linear_algebra/matrix_utils.f90。

- 命名與可重現性
  - 測試檔與模組檔應具一致性，例如 test/matrix_utils_test.f90 對應模組 matrix_utils。
  - 在專案中避免以日期、版本號混入檔名，改以功能描述為主，方便長期維護。
  - 版本控管下，檔名變動應與程式行為變動相呼應，確保最小可重現案例容易取得。

- 實作與範例
  - 下列範例展示如何規劃目錄與檔案，以及如何用簡單的 Makefile 結合命名約定。
  
  ```
  # 建立統一的專案目錄
  mkdir -p project/src project/include project/tests project/bin
  ```

  ```
  # src/matrix_utils.f90
  module matrix_utils
    implicit none
  contains
    function mat_norm(A) result(n)
      real, intent(in) :: A(:,:)
      real :: n
      n = sqrt(sum(A**2))
    end function mat_norm
  end module matrix_utils
  ```

  ```
  # tests/test_matrix_utils.f90
  program test_matrix_utils
    use matrix_utils
    implicit none
    real :: A(2,2), n
    A = reshape([1.0,2.0,3.0,4.0],[2,2])
    n = mat_norm(A)
    if (abs(n - 5.4772256) > 1e-6) then
      print *, "Test failed: norm mismatch"
    else
      print *, "Test passed"
    end if
  end program test_matrix_utils
  ```

  ```
  # Simple Makefile 條列，利於穩定編譯與命名管理
  FC = gfortran
  SRC = src/matrix_utils.f90
  TEST = tests/test_matrix_utils.f90
  BIN  = bin/run_matrix_utils

  all: $(BIN)

  $(BIN): $(SRC)
    mkdir -p bin
    $(FC) -o $(BIN) $(SRC)

  test: $(SRC) $(TEST)
    $(FC) -o bin/test_matrix_utils $(SRC) $(TEST)
  ```

- 版本控制中的實務
  - 建議在 .gitignore 中排除編譯輸出，僅將 src、include、tests、doc 等源檔與設置提交。
  - Commit 訊息以動作開頭，如 feat(build): 設置專案結構與命名規範，fix(naming): 修正模組檔與檔名不一致問題。

小結與過渡
透過上述檔案與命名約定，團隊可以快速建立穩定的開發環境與可重現的測試案列。這不僅有助於新進人員理解專案架構，也為日後的除錯與版本控管奠定基礎。接下來我們將進入「具互動性的第一個版本」，練習使用者輸入與格式化輸出，並開始建立簡易測試與除錯的習慣，讓程式更穩健地與使用者互動。

### 節5：撰寫與執行範例程式

在本節，我們以互動為核心，實作一個能與使用者輸入互動並輸出格式化結果的小範例。透過這個練習，理解輸入輸出流程、格式化字串的基本用法，以及初步的測試與除錯思維。為了強調數學與程式的連結，先把核心公式寫清楚：$Area = π r^2$，且在文內以行內公式表示為 $Area = \pi r^2$，若需要更清晰的視覺效果，可在區塊中寫成
$$
Area = \pi r^2
$$

以下程式為 Fortran 90/95 的自由格式寫法，目的費用介面友善，並包含基本輸入驗證與格式化輸出。程式會請使用者輸入名字與半徑，計算圓面積並以易讀的格式輸出。

```fortran
! interactive_example.f90
program interactive_example
  implicit none
  character(len=50) :: name
  real :: radius, area
  real, parameter :: pi = 3.14159265358979323846

  print *, "請輸入你的名字："
  read(*,'(A)') name
  print *, "請輸入半徑（公分）："
  read(*,*) radius

  if (radius < 0.0) then
     print *, "錯誤：半徑不能為負！"
     stop 1
  end if

  area = pi * radius * radius

  write(*,'(A, 1X, A)') 'Hello', trim(name)
  write(*,'(A, 1X, F8.2)') 'Area =', area
end program interactive_example
```

編譯與執行的步驟如下，若你使用的是 GCC 的 Fortran 編譯器 gfortran，請在終端機輸入：
- `gfortran -Wall -Wextra -o interactive_example interactive_example.f90`
- `./interactive_example`

執行後的對話範例如下：
請輸入你的名字：
Alice
請輸入半徑（公分）：
5
Hello Alice
Area = 78.54

這樣的範例讓新手直接感受到「互動輸入」與「格式化輸出」的結合，並能清楚看到輸出格式的佈局。為了養成基本的測試與除錯習慣，可以在程式中加入簡單的檢查與日誌輸出，例如在輸入不符合條件時顯示友善的錯誤訊息、在關鍵步驟輸出中間結果以便追蹤。

進階建議
- 使用 -Wall 與 -Wextra 以捕捉未使用的變數、未初始化值等警告。
- 將輸出格式化分離到 format/i/o 規則，日後改成子程序以利重用。
- 練習將程式分成模組與主程序，提升可維護性與測試性。
- 增設簡單的自動化測試：提供多組半徑輸入，驗證輸出是否符合預期的字串與數字格式。

透過這個「第一個互動版本」的練習，我們建立了基礎的輸入-輸出流程與除錯習慣。接下來，將在「強化輸入驗證與容錯處理」部分，延伸到更穩健的使用者互動與資料處理。

## 第 3 章：章三：基本語法與資料型態概覽

### 節1：變數、常數與命名規則

在 FORTRAN 的零基礎學習中，變數與常數的正確使用是後續輸入輸出與運算穩定性的基礎。為避免隱式型別帶來的怪異行為，建議開頭就使用 implicit none，強制每個變數必須顯式宣告。常見型別包括整數 integer、實數 real（含單精度 real、雙精度 real(kind=8) 等）、複數 complex、字串 character 與邏輯值 logical。為了未來的可攜性與可讀性，建議以 kind 來指定精度，並把常數以 parameter 宣告為不可變的常量。

首先，常見的型別宣告範例（使用 Fortran 90/95 風格）：
- 整數與實數型別
- 以 real64 表示雙精度實數，使用 ISO_FORTRAN_ENV 的 kind 常數
- 使用 parameter 宣告常數

為了讓程式的行為更穩健，建議把輸入變數與常數的位階分離，並以模組管理型別與常數。下列範例展示如何以模組保存型別與常數定義，主程式再使用這些定義進行運算。

範例程式：模組化的變數、常數與命名
```fortran
module var_def
  use iso_fortran_env, only: real64, int64
  implicit none
  private
  public :: n, radius, PI, ok, x, y

  integer(int64)  :: n
  real(real64)     :: radius, x, y
  real(real64), parameter :: PI = 3.1415926535897932384626433832795_real64
  logical           :: ok
end module var_def
```

主程式使用模組中的變數與常數，並示範基本命名規則與起始位元 0 的陣列範例：
```fortran
program demo_names
  use var_def
  implicit none

  real(real64), dimension(0:9) :: points
  integer(int64) :: i

  n = 10
  radius = 1.0_real64
  ok = .true.

  do i = 0, n-1
     points(i) = i * 0.5_real64
  end do

  print *, 'Radius = ', radius
  print *, 'PI = ', PI
  print *, 'Awarded points:'
  print '(10F6.2)', points

end program demo_names
```

命名規則的實務要點
- 運用描述性名字：radius、length、max_count、user_name 等，避免單字母及過於模糊的名稱。
- 具備區分度的命名長度：在現代編譯器裡，名稱長度通常允許到 63 個字元，但實務上以 20–40 字元為佳，避免過長導致維護困難。Fortran 的名字在語法層面通常對大小寫不敏感，為利可讀性，建議以大寫或一致風格撰寫。
- 識別性的前綴與後綴：使用 radius_、n_total、flag_ok 等，清晰區分變數的用途與型別。若採用前綴，請在整個專案保持一致性。
- 避免保留字與衝突：避免使用程式語言的保留字作為變數名；如若偶爾需要，則可在模組內部使用私有(private)與公有(public)的界線，避免外部呼叫者誤用。
- 起始字元與陣列索引說明：Fortran 的識別字通常以字母開頭，可使用 0 起始的陣列，例如 dimension(0:9)；這在零基礎教學中尤為重要，需在說明中標註。

程式設計中的風格與容錯
- 強制宣告與模組化：implicit none 與模組化有助於在編譯階段抓錯，例如未宣告的變數、打錯型別等。
- 常量的定義與單一來源：將所有可信賴的數值常量集中在模組中，以 parameter 宣告；避免在多處硬編碼與重複定義。
- 命名自我說明：即使是常數 PI，也建議用大寫與清楚含義表達，避免使人混淆為其他符號。

本節的設計原則是把變數、常數與命名規則與後續的模組化設計、輸入檢核與容錯處理串接起來。下一節將進一步強化輸入驗證的技巧，例如使用 explicit 採樣與範圍檢查，並介紹如何透過封裝與介面提高測試性與穩定性，讓使用者互動更可控且可預期。透過這些規劃，我們逐步建立穩健的輸入-輸出流程與除錯習慣，為更高階的資料處理奠定基礎。

### 節2：整數、實數與布林值的基本型別

在本節，我們聚焦三大基本型別：整數（INTEGER）、實數（REAL/REAL(KIND=...)）與布林值（LOGICAL）。為了日後的模組化設計與輸入檢核奠定基礎，我們會同時談命名規則、常數的管理，以及如何在實作中避免隱性型別造成的錯誤。為了清楚呈現，常數 PI 也會以大寫的 PI 命名，並以清楚含義表示，以避免與其他符號混淆。

1) 整數型別（INTEGER）
- 作用與範圍：整數用於計數、索引與離散資料。不同系統的位元數可能不同，推薦常以 kind 指定寬度，確保跨平台的一致性。
- 型別與宣告範例：
  - 基本寫法：INTEGER :: i
  - 指定寬度：INTEGER, PARAMETER :: I32 = selected_int_kind(9)
  - 使用時：INTEGER(KIND=I32) :: n
- 範圍與溢位：範圍依資料型別而異，常見情況為約 $\pm2^31−1$ 的整數可用於 32 位元整數。為避免溢位，宜在程式一開始就用 explicit kind。
- 小提醒：避免「隱性型別」的風險，建議啟用 implicit none，並使用 explicit kind。

1) 實數型別（REAL）
- 作用：實數用於連續量，例如測量與計算結果。預設 REAL 通常為單精度，精度與範圍有限，需留意舍入誤差。
- 指定寬度與常數：使用 dp（double precision）等字元來提升精度：
  - real(kind=dp) :: x
  - integer, parameter :: dp = selected_real_kind(15, 307)
  - real(kind=dp), parameter :: PI = 3.1415926535897932384626433832795_dp
- 常見運算中的注意事項：浮點數比較要避免直接等於，可使用允許誤差的判定。若需要更高穩定性，考慮使用雙精度與模組化介面。
- 公式示例：區間與誤差可表示為  
  $$|x - y| < \\varepsilon \quad\\text{且}\\quad \\varepsilon \approx 10^{-12}$$  
  ，以說明實數運算的近似性。

1) 布林值（LOGICAL）
- 用途：條件分支與狀態旗標的表示，值域為 .TRUE. 與 .FALSE.。
- 宣告與使用範例：
  - LOGICAL :: ok
  - ok = .TRUE.
  - IF (ok) THEN ... END IF
- 注意：在 Fortran 中布林常數使用點（.TRUE.、.FALSE.），與整數/實數型別分別獨立。

1) 常數與命名規則
- 常數宣告：使用 PARAMETER 將不會改變的值固定下來，便於測試與維護。
- 命名建議：常數以大寫並使用具描述性的名字；變數採用易讀的中文解釋變數名或英文字母搭配底線。
- 常數範例：PI 常數的正確使用方式（必須用高可讀的寫法與明確的型別）：
  - integer, parameter :: dp = selected_real_kind(15, 307)
  - real(kind=dp), parameter :: PI = 3.1415926535897932384626433832795_dp
- 公式表示：$PI \approx 3.1415926535897932384626433832795$

1) 小型範例程式：型別與輸入基本用法
以下程式展示變數宣告、常數、布林值，以及一點點輸入/輸出與 IOSTAT 檢查，為日後的輸入驗證鋪路。

```fortran
! 範例：基本型別與 IOSTAT 檢查
program type_demo
  implicit none
  integer, parameter :: dp = selected_real_kind(15, 307)
  integer, parameter :: I32 = selected_int_kind(9)
  integer(kind=I32) :: n
  real(kind=dp)    :: x
  logical          :: ok
  integer          :: ierr

  ! 以顯示說明的方式設定 PI 常數
  real(kind=dp), parameter :: PI = 3.1415926535897932384626433832795_dp

  print *, "請輸入一個整數與一個實數（用空格分隔）：" 
  read(*,*,IOSTAT=ierr) n, x
  if (ierr /= 0) then
     print *, "輸入格式錯誤，請輸入整數與實數。"
  else
     ok = .TRUE.
     print *, "您輸入的整數 n =", n
     print *, "您輸入的實數 x =", x
     print *, "圆周率 PI ≈ ", PI
  end if
end program type_demo
```

再提供另一個簡單布林值範例，用於條件分支的練習：

```fortran
program boolean_demo
  implicit none
  logical :: flag
  flag = .TRUE.
  if (flag) then
     print *, "布林值為 True"
  else
     print *, "布林值為 False"
  end if
end program boolean_demo
```

過渡到下一節，我們將把以上內容推展到更嚴謹的輸入檢核與介面設計，例如使用 explicit sampling、範圍檢查，以及如何用模組化把整型、實型與邏輯型的操作封裝成可重用的介面，讓測試與除錯更加穩定與可預期。透過這些規劃，接下來的章節會逐步建立穩健的輸入–輸出流程與容錯機制，為更高階的資料處理奠定基礎。

### 節3：字元與字串處理

在 Fortran 中，字元與字串是程式語言最常用的資料型態之一。與很多現代語言不同，Fortran 的字串預設為固定長度，也就是你宣告的字串長度決定了它能放多少個字元；未使用完的部分會以空格填補。這讓字串的長度、尾端空格、以及如何拼接與裁剪成為學習的核心。

- 基本概念
  - 文字型別：CHARACTER(len=…) 是最常見的字元資料型態。若宣告為長度 12，該字串就能存放最多 12 個字元，超過部份會受到截斷。
  - 字串常量：用單引號或雙引號括起，如 'FORTRAN' 或 "FORTRAN"。兩種寫法等效，但建議統一以避免混淆。
  - 固定長度與尾端空格：例如字串 s = 'HELLO'，實際儲存長度若是 len(s)=12，字串內部會是 'HELLO' 後面跟著 7 個空格。

- 典型操作與內建函式
  - 字串長度：LEN(s) 可得到字串的宣告長度，如 LEN('HELLO') = 5，但若 s 是長度 12 的變數，LEN(s) 回傳仍是 12。
  - 去尾空格：LEN_TRIM(s) 回傳去掉尾端空格後的長度；TRIM(s) 回傳去尾端空格的字串本體。
  - 對齊與調整：ADJUSTL(s) 將字串左對齊，ADJUSTR(s) 將字串右對齊，兩者會在中間補空格使長度保持不變。
  - 取子字串：s(i:j) 取自第 i 個到第 j 個字元（Fortran 的索引從 1 開始）。
  - 拼接：字串用 // 運算符進行拼接，例如 a // b。
  - 查找與分割：INDEX(s, t) 回傳 t 在 s 中的第一個出現位置，若未找到則回傳 0。Fortran 亦支援 SCAN、VERIFY 等函式族，分別用於搜尋與驗證，實務上可用於簡單的字串處理與分割。
  - 重複字串：REPEAT(s, n) 回傳 s 重複 n 次的結果。
  - 字串比對：直接用比較運算子，如 if (s == t) then …，也可用於簡單的字串等值比較。

- 輸入與輸出時的注意
  - 輸入整行：若要讀取整行並保留中間的空白，使用格式化的輸入，如 READ(*,'(A)') line。若改用 LIST- DIRECT 輸入 (READ(*,*))，則會依照分隔字元進行拆分，較難取得整行。
  - 輸出時的長度控制：因為變數有固定長度，直接輸出時容易出現尾端空格。常用做法是先用 TRIM 或 LEN_TRIM 去取得實際內容，再輸出。

- 範例與實作要點
  - 以固定長度的字串為例，展示 LEN、LEN_TRIM、TRIM、ADJUSTL、ADJUSTR 等的差異；同時示範子字串與拼接的基本用法。
  - 也展示如何把輸入的整行字串做前後去空與對齊，讓顯示結果整潔。

- 小技巧與實作練習
  - 將多個欄位的字串組成報表時，先把每個字串用 TRIM 與 ADJUSTL 去空齊整，最後再用 DELIM 與 CONCAT 取得整體排版。
  - 綜合使用 LEN_TRIM、INDEX 與 SUBSTRING，可以從一行文字抽取關鍵欄位，例如把以逗號分隔的資料分解成多個欄位。

下面給出一個簡單的示範程式，說明固定長度字串的基本用法與常見操作。程式中包含字串的宣告、拼接、子字串、尋找、以及讀入整行輸出整理的流程。

```fortran
program string_demo
  implicit none
  character(len=12) :: s = 'FORTRAN     '
  character(len=12) :: t = 'LANGUAGE    '
  character(len=24) :: combined
  integer :: p, L

  ! 基本長度與去尾空格
  print *, '原始 s = "', s, '", LEN(s) = ', len(s)
  print *, 'len_trim(s) = ', len_trim(s)
  print *, 'trim(s) = "', trim(s), '"'

  ! 對齊與拼接
  print *, 'adjust left:  "', adjustl(s), '"'
  print *, 'adjust right: "', adjustr(s), '"'
  combined = s // ' ' // t
  print *, 'combined = "', combined, '"'
  print *, 'len(combined) = ', len(combined)

  ! 子字串與尋找
  print *, 'sub s(2:5) = "', s(2:5), '"'
  p = index(s, 'FOR')
  print *, 'index(s, "FOR") = ', p

  ! 讀入整行，並輸出整理後的內容
  character(len=100) :: line
  print *, '請輸入一整行文字：'
  read(*,'(A)') line
  print *, '你輸入的整理結果： ', trim(line)

end program string_demo
```

在這個範例中，字串 s 的宣告長度固定為 12；透過 trim 與 len_trim 可以取得實際內容長度，避免尾端空格干擾輸出與比對。子字串、拼接與尋找的操作都以直覺的語法呈現，讓零基礎的讀者能快速建立對字串處理的信心。

作為後續章節的鋪墊，字元與字串處理的穩健設計，將直接影響你在輸入與輸出流程中的容錯與可預期性。接下來，我們將把這些字串技巧整合到更嚴謹的輸入檢核與介面設計之中，透過 explicit sampling、範圍檢查，以及模組化的介面設計，使整個測試與除錯流程更穩定。透過這些規劃，接下來的章節會逐步建立穩健的輸入–輸出流程與容錯機制，為更高階的資料處理奠定基礎。

### 節4：陣列與向量的起步

在前一小節我們已經建立了對字串與基本資料型態的信心。現在要把焦點轉向陣列與向量的起步，因為它們是輸入與輸出流程中的核心容器。為了支援日後的零基礎輸入檢核，我們先把可預期的索引與大小管理好，讓程式在各種資料與介面上都能穩定地工作。

- 陣列與向量是同形的多個資料元素組成的集合。Fortran 常見的寫法是 dimension(下界:上界)，例如 dimension(0:9) 表示有 10 個元素，索引從 0 到 9。若不指定下界，預設通常是 1，稱作 1-based。計算元素個數時，可用公式
  $$
  N = \mathrm{UBOUND}(a,1) - \mathrm{LBOUND}(a,1) + 1
  $$
  其中 $\mathrm{UBOUND}$ 與 $\mathrm{LBOUND}$ 分別回傳上界與下界。若下界為 0，則 $N = 9 - 0 + 1 = 10$。這些概念對容錯必備，能讓我們在輸入時避免越界。

4.1 基本概念與語法要點
- 向量與陣列的元素可以直接相加減乘除，形成元素級運算。例如，兩個相同形狀的向量相加，會得到一個同樣形狀的新向量：
  $$
  c = a + b
  $$
  這是 Fortran 的元素級運算特性。
- 陣列元素的存取藉由索引完成，若下界是 0，合法的索引區間也必須以 0 為起點。

4.2 靜態宣告與動態配置
- 靜態陣列（已在編譯時定義大小）適合固定長度的資料，向量長度在編譯時確定。
- 動態陣列透過 allocatable 型別與 allocate 來建立，便於執行時決定長度。

以下分別示範靜態與動態的零基礎陣列宣告與填值：

```fortran
! 靜態：零基礎陣列範例
program arr_zero_based
  implicit none
  integer, parameter :: N = 5
  integer, dimension(0:N-1) :: a
  integer :: i

  do i = 0, N-1
     a(i) = i*i
  end do

  print *, "a = ", a
end program arr_zero_based
```

```fortran
! 動態：以 0 ~ n-1 宣告與填值
program arr_alloc
  implicit none
  integer, allocatable :: b(:)
  integer :: n, i

  n = 6
  allocate(b(0:n-1))
  do i = 0, n-1
     b(i) = i + 1
  end do

  print *, "b = ", b
  deallocate(b)
end program arr_alloc
```

4.3 基本運算與向量化
- 陣列可以直接作為整體進行運算，Fortran 會對應元素進行計算。也可使用內建函式對向量做常見處理，如點積、總和、展開等。
- 常見的輸出與檢查方法，配合 LBOUND/UBOUND，能避免越界錯誤。

```fortran
program vec_ops
  implicit none
  integer, parameter :: N = 5
  integer, dimension(0:N-1) :: x, y, z
  integer :: i

  do i = 0, N-1
     x(i) = i
     y(i) = 2*i
  end do
  z = x + y  ! element-wise addition
  print *, "x = ", x
  print *, "y = ", y
  print *, "z = ", z
end program vec_ops
```

4.4 取值和區段的實務
- 你可以取整段陣列作為新陣列，使用區段表示法。下界若為 0，區段也需以 0 作為起點。
- 內建函式如 DOT_PRODUCT、SIZE、RESHAPE 可用於簡化矩陣與向量運算；同形向量可進行點積等運算。

```fortran
program bounds_demo
  implicit none
  integer, dimension(0:4) :: arr
  integer :: l, u, n

  l = LBOUND(arr, 1)
  u = UBOUND(arr, 1)
  n = u - l + 1
  print *, "bounds:", l, u, "size:", n
end program bounds_demo
```

4.5 以 I/O 的穩健輸入與輸出為基礎
- 讀入動態長度的陣列時，先取得長度再建立對應的下界與上界，避免硬編碼造成的錯誤。
- 讀取與輸出時，建議先顯示索引範圍與元素個數，讓使用者或測試資料更容易對照。

```fortran
program read_array
  implicit none
  integer, allocatable :: a(:)
  integer :: n, i

  print *, "Enter number of elements:"
  read(*,*) n
  allocate(a(0:n-1))

  do i = 0, n-1
     print *, "Enter element ", i, ":"
     read(*,*) a(i)
  end do

  print *, "You entered: ", a
  deallocate(a)
end program read_array
```

透過上述範例與概念，現在你應該能自信地在零基礎的前提下設計與操作 Fortran 的陣列與向量。下一節將把這些陣列運用到更嚴謹的檔案輸入與介面設計，進一步強化輸入檢核與容錯能力，並引入模組化的介面設計，為更高階的資料處理奠定穩固的基礎。

### 節5：輸入與輸出（I/O）的基本用法

在零基礎的 Fortran 入門中，I/O 是與使用者與外部世界互動的第一道門。透過 PRINT、READ、WRITE，以及 OPEN 等語句，您可以把資料顯示在螢幕上、從鍵盤取得使用者輸入，或是與檔案進行互動。常見的方式分為兩大類：list-directed I/O（以逗號分隔的直覺輸入輸出）與格式化 I/O（以自訂格式字串控制輸出樣式）。若不特別指定，Fortran 的 I/O 常採用 list-directed 形式，適合初學者快速練習。

- 基本觀念
  - PRINT 與 WRITE 用於輸出；READ 用於輸入。等同於其他語言的 print/read，但語法與格式控制略有差異。
  - LIST-Directed I/O（如 READ(*,*)、WRITE(*,*)）使用預設格式，方便快速嘗試。
  - FORMAT 讓你自訂輸出樣式，例如寬度、位數、字串對齊等。
  - OPEN/READ/WRITE/CLOSE 讓資料可與檔案互動；I/O 錯誤可透過 IOSTAT、ERR、END、IOSTAT 來偵測與處理。

- 以向量輸入輸出為例的基本練習
  下列範例展示如何從鍵盤取得整數 n，動態配置一個實數向量，接著輸入 n 個數字，最後把整個向量輸出。此處使用 list-directed I/O，適合初學者。

  ```fortran
  program io_basic
    implicit none
    integer :: n, i
    real, allocatable :: a(:)

    print *, "請輸入陣列大小 n："
    read(*,*) n

    if (n <= 0) then
       print *, "無效的大小。"
       stop
    endif

    allocate(a(n))
    print *, "請輸入 ", n, " 個實數："
    read(*,*) (a(i), i=1,n)

    print *, "您輸入的向量為："
    print *, (a(i), i=1,n)

    deallocate(a)
  end program io_basic
  ```

  同樣的原理也可以用格式化輸出讓輸出更整齊：

  ```fortran
  program io_format
    implicit none
    real :: x = 12.3456
    print *, "原始數值："
    write(*,'(A,F6.2)') "值 = ", x
  end program io_format
  ```

- 檔案輸入輸出的基礎
  對於長期使用的資料，檔案 I/O 提供穩定的資料持久化能力。以下範例示意先寫入再讀取：

  ```fortran
  program io_file
    implicit none
    integer :: ios, unit
    real :: v
    character(len=100) :: fname = 'sample.txt'

    unit = 10
    open(unit=unit, file=fname, status='REPLACE', action='WRITE', iostat=ios)
    if (ios /= 0) then
       print *, "開檔寫入失敗，I/O 狀態：", ios
       stop
    end if

    v = 3.14159
    write(unit, *) v
    close(unit)

    open(unit=unit, file=fname, status='OLD', action='READ', iostat=ios)
    if (ios /= 0) then
       print *, "開檔讀取失敗，I/O 狀態：", ios
       stop
    end if

    read(unit, *) v
    close(unit)
    print *, "檔案內容讀出：", v
  end program io_file
  ```

- 錯誤處理與容錯
  實務上常需要知道這次 I/O 是否成功。透過 IOSTAT 可以取得錯誤碼，並用 ERR、END 轉跳處理 EOF 或錯誤情況。範例：

  ```fortran
  program io_check
    implicit none
    real :: x
    integer :: ios

    print *, "請輸入浮點數："
    read(*,*, iostat=ios) x
    if (ios /= 0) then
       print *, "輸入發生錯誤，IOSTAT=", ios
       stop
    end if
    print *, "您輸入的數字是：", x
  end program io_check
  ```

- 未格式化與格式化的取捨
  未格式化輸出（list-directed）最直覺，適合教學與快速驗證；格式化輸出適合追求對齊與易讀性的報表。例如用格式字串可以控制欄寬、浮點精度，甚至混合字串與數值的排版。若需要可在公式層面表示輸出規則時，會用到如 $mean = \frac{\sum x_i}{n}$ 這樣的表達式，方便在文字說明中呈現計算邏輯。

結尾的重點是，先掌握基本的輸入輸出流程與簡單的錯誤偵測，日後再把這些 I/O 能力與檔案介面、模組化介面結合，進行更嚴謹的資料處理與資料介面的設計。接下來的節次將引入更嚴謹的輸入檢核與容錯機制，並開始模組化的介面設計，為高階的資料處理奠定穩固的基礎。

# 第 2 篇：第二篇：FORTRAN 基礎語法與流程控制

## 第 1 章：章四：流程控制：條件與迴圈

### 節1：IF-ELSE 的基本用法

在 FORTRAN 的流程控制中，IF-ELSE 是最常見也最基本的分支機制。基本結構是：IF (條件) THEN … ELSEIF (條件) THEN … ELSE … END IF。條件可以是簡單的比較式，如 x > 0，也可以是多條件組合，如 (x > 0) .AND. (y < 10)。為了幫助新手避免變數未宣告等問題，建議在程式一開始加入 implicit none，並在需要的地方進行明確宣告。

輸入輸出與錯誤偵測是本章的重點之一。利用 IF-ELSE 進行輸入範圍檢查、型別檢查與錯誤提示，可以讓程式在遇到不可預期輸入時給出友善的回報，避免崩潰或產出錯誤結果。以下範例展示單條件與多條件的基本用法，並結合輸入檢核。

範例 1：單條件判斷與輸入檢核
```fortran
program if_else_basic
  implicit none
  integer :: score

  print *, '請輸入成績（0-100）：'
  read *, score

  if (score < 0 .or. score > 100) then
     print *, '輸入錯誤：成績必須介於 0 與 100 之間。'
  else if (score >= 60) then
     print *, '及格'
  else
     print *, '不及格'
  end if
end program
```

範例說明：
- 使用 implicit none 強制變數宣告，降低打字錯誤。
- 使用 .or. 連結兩個範圍檢查，確保分數落在合理範圍內。
- ELSEIF 用於在同一次輸入下分階段回報，提供清晰回饋。

範例 2：多條件與巢狀判斷
```fortran
program compound_conditions
  implicit none
  integer :: a, b

  print *, '請輸入兩個整數:'
  read *, a, b

  if (a > 0 .and. b > 0) then
     print *, '兩數皆為正'
  else if (a > 0 .or. b > 0) then
     print *, '至少有一個正數'
  else
     print *, '兩數皆為非正'
  end if
end program
```

範例說明：
- .and. 與 .or. 用於組合條件，括號可用以控制評估順序。
- 巢狀的 ELSEIF 讓使用者在單一執行週期內得到更細緻的結果分類。

另附：零基礎概念的輔助說明。FORTRAN 的陣列預設是以 1 為起始下標，但你也可以顯式定義低界，如 dimension(0:4) 的陣列，即可使用 arr(0) 開始存取。這對於理解“零起點”在某些演算法中的角色有幫助，並不影響 IF-ELSE 的基本語法，但在實作上要注意下標範圍，避免指標越界。

在流程設計層面，IF 的判斷式通常會搭配輸出訊息與錯誤提示，例如：
- 對於不合法的輸入，先給出錯誤訊息，再終止或要求重新輸入。
- 對於邊界值，給予明確的範圍說明，避免模糊回饋。

公式與思考的連結。若要以數學角度理解條件評估，可以把整體決策視為在不同區間內落入不同結果的分段函數。例如，平均值的計算可寫成一個簡單的公式：  
$$
mean = \frac{\sum x_i}{n}
$$  
在 FORTRAN 程式中，若要顯示這一公式的意義，可以先用變數表示總和與個數，再在符合條件時輸出結果與公式說明。更一般地，若你需要清楚地表示計算過程，可在註解或輸出中寫成：  
$$
mean = \frac{1}{n}\sum_{i=1}^{n} x_i
$$
以協助讀者理解數學與程式之間的對應關係。

小結與注意事項：
- 使用 implicit none 與明確宣告變數，是提升穩定性的好習慣。
- 條件式可結合 .AND.、.OR. 與括號，避免歧義。
- 適度的錯誤訊息與輸入檢核，是日後模組化介面的基礎。

過渡到下一節，將更深入地探討嚴謹的輸入檢核與容錯機制，並開始模組化介面設計，為高階的資料處理與介面互動奠定穩固的基礎。下一節我們將聚焦於如何避免非數字輸入、處理極端值，以及在多模組中維持一致的 I/O 行為。

### 節2：複合條件與邏輯運算

在前一小節中，我們學會如何以單一條件做判斷。實務上往往需要把多個條件綁在一起，才能正確決定程式的走向。Fortran 提供了三個核心的邏輯運算符：.AND., .OR. 與 .NOT.，並且可以透過括號清楚地定義運算順序與優先權。為了避免歧義，請務必使用括號把複合的邏輯結構寫清楚。以下要點值得注意：

- 布林值與布林表達式：布林值在 Fortran 中由邏輯型別表示，常見的寫法是把判斷結果直接指派給一個 LOGICAL 變數，或直接放在 IF 判斷式裡。  
- 運算順序與括號：NOT 的效果通常優先於 AND 與 OR，若想要明確的順序，請使用括號。例如：(A .AND. B) .OR. C vs A .AND. (B .OR. C) 會得到不同結果。  
- 避免短路依賴：Fortran 的條件運算不一定保證短路求值，因此右側子表達式若有副作用（例如子函式呼叫、I/O 操作），不要把副作用放在沒被先檢查的路徑上。若需要確保先檢查第一個條件再評估第二個，請使用巢狀 IF。

- 複合條件的最佳實務：用中介變數清楚呈現意圖，並以括號避免誤解。  
  公式層面理解時，可以把布林運算轉寫成數學形式幫助理解：
  - 行為對映為：$(A \land B) \lor C$，對應於 Fortran 的 $(A .AND. B) .OR. C$。  
  $$ (A \land B) \lor C $$

- 設計小結：先用單一條件測試正確性，接著逐步加入條件，並以中介變數提升可讀性與後續維護性。

以下提供兩個實作範例，說明「單一表達式」與「中介布林變數」的寫法差異。

範例一：直接寫在單一 IF 條件中的複合條件
```fortran
program comp_cond_direct
  implicit none
  integer :: age
  real    :: income
  integer :: has_disability
  logical :: eligible

  print *, "請輸入年齡、收入、是否有殘障(1/0)："
  read *, age, income, has_disability

  ! 複合條件：年滿18且收入>=12000，或有殘障
  eligible = ((age >= 18) .AND. (income >= 12000.0)) .OR. (has_disability == 1)

  if (eligible) then
     print *, "符合資格。"
  else
     print *, "不符合資格。"
  end if
end program comp_cond_direct
```

範例二：使用中介布林變數提升可讀性
```fortran
program comp_cond_intermediate
  implicit none
  integer :: age
  real    :: income
  integer :: has_disability
  logical :: age_ok, income_ok, disability

  print *, "請輸入年齡、收入、是否有殘障(1/0)："
  read *, age, income, has_disability

  age_ok      = (age >= 18)
  income_ok   = (income >= 12000.0)
  disability  = (has_disability == 1)

  ! 組合條件：以中介變數呈現每一個子條件，再做整體判斷
  if ((age_ok .and. income_ok) .or. disability) then
     print *, "符合資格。"
  else
     print *, "不符合資格。"
  end if
end program comp_cond_intermediate
```

延伸補充與實務建議
- 為避免歧義，先把單一判斷寫出來，接著再逐步組成複合條件，這樣更容易追蹤錯誤來源。  
- 嘗試用顯式的布林變數命名，如 age_ok、income_ok、valid_group，能迅速讓閱讀者理解條件的邏輯意圖。  
- 在多模組介面中，保持一致的 I/O 行為與表達風格，避免因條件寫法不同而造成相容性問題。

結尾過渡：藉由以上對複合條件與邏輯運算的清楚與穩健寫法，下一節我們將更深入探討嚴謹的輸入檢核與容錯機制，並開始模組化介面設計，為高階的資料處理與介面互動打下穩固的基礎。接著，我們會聚焦於如何避免非數字輸入、處理極端值，以及在多模組中維持一致的 I/O 行為。

### 節3：SELECT CASE 的用法與範例

SELECT CASE 是 Fortran 提供的條件分支之一，透過 select 或 selector 表達式的值，對多個 CASE 區塊進行匹配。與長串的 IF ELSE 相比，SELECT CASE 的結構更清晰、易於閱讀，尤其在多模組介面中能維持一致的 I/O 行為與表達風格。關鍵要點是：CASE 的標籤型別必須與 selector 相容，且通常以整數與範圍表示為主；CASE DEFAULT 表示未命中前述所有 CASE 的情況。下列範例展示常見用法與寫法風格。

- 語法要點
  - 基本格式：
    - ```
      SELECT CASE (expr)
      CASE (label)
      CASE (start:end)
      CASE DEFAULT
      END SELECT
      ```
  - CASE 內可放多條語句，執行完該區塊後跳出 SELECT CASE，繼續之後的程式。
  - 不會像某些語言那樣自動穿透到下一個 CASE；範圍與值的搭配需清楚無歧義。

以下提供兩個範例，說明常見場景與寫法風格。

```fortran
! 範例1：以成績分級
program grade_by_score
  implicit none
  integer :: score
  character(len=1) :: grade

  print *, "Enter score (0-100):"
  read *, score

  select case (score)
  case (90:100)
     grade = 'A'
  case (75:89)
     grade = 'B'
  case (60:74)
     grade = 'C'
  case default
     grade = 'D'
  end select

  print *, "Grade = ", grade
end program
```

```fortran
! 範例2：以單一代碼對應多執行路徑
program option_lookup
  implicit none
  integer :: code
  character(len=20) :: desc

  print *, "Enter code (1-3):"
  read *, code

  select case (code)
  case (1)
     desc = "Initial"
  case (2)
     desc = "Processing"
  case (3)
     desc = "Completed"
  case default
     desc = "Unknown"
  end select

  print *, "Status: ", trim(desc)
end program
```

- 小結與設計要點
  - 使用 CASE (start:end) 可清楚表達數值區間，提升可讀性。
  - 對於多模組介面，統一的 CASE 結構與輸出格式有助於降低相容性問題。
  - 若需要對同一結果再分支，可在同一 CASE 區塊內放多條語句，避免額外的流程跳轉。
  - 對非預期輸入，使用 CASE DEFAULT 提供穩健的預設行為，避免未捕捉的錯誤流造成程序崩潰。

- 額外的數學表示（LaTeX 符號）
  內部邏輯的對應可以以簡單的條件函式呈現，如下所示。
  $$
  GRADE =
  \begin{cases}
  A, & 90 \le score \le 100 \\
  B, & 75 \le score \le 89 \\
  C, & 60 \le score \le 74 \\
  D, & score < 60
  \end{cases}
  $$
  透過這樣的映射，我們可以直觀地理解各分數區間對應的等級，與 SELECT CASE 的對應區塊相呼應。

結尾過渡：藉由以上對 SELECT CASE 的清楚與穩健寫法，下一節我們將更深入探討嚴謹的輸入檢核與容錯機制，並開始模組化介面設計，為高階的資料處理與介面互動打下穩固的基礎。

### 節4：DO 迴圈與迭代控制

DO 迴圈是 Fortran 中最常用的迭代結構，適合用於固定區間的逐步執行。基本語法為 DO variable = start, end, step，當 step 省略時，預設為 1；迴圈結束以 END DO 為界。若 start、end 與 step 的組合不滿足執行條件，該 DO 不會執行一次。為了估算迭代次數，可以用公式推導：若 step > 0 且 end >= start，n 為 $n = \left\lfloor \dfrac{end - start}{step} \right\rfloor + 1$；若條件不成立，則 n = 0。這在設計前置條件與容量分配時相當實用。
$$ n = \left\lfloor \frac{end - start}{step} \right\rfloor + 1 $$

1) 基本語法與範例
在最常見的情境中，用一個整數 i 走訪 1 到 10，逐次輸出：

```fortran
program basic_do
  implicit none
  integer :: i
  do i = 1, 10
     print *, "i =", i
  end do
end program basic_do
```

2) 自訂步長與方向
若要以步長 2 往前或往後走，可指定 step：

```fortran
program step_do
  implicit none
  integer :: i
  do i = 1, 9, 2
     print *, "i =", i
  end do
end program step_do
```

3) DO WHILE 的使用
Fortran 90 以後加入 DO WHILE，使迴圈在符合條件時持續執行，條件為括號內的邏輯式：

```fortran
program do_while_demo
  implicit none
  integer :: x
  x = 0
  do while (x < 20)
     x = x + 4
     print *, "x =", x
  end do
end program do_while_demo
```

4) EXIT 與 CYCLE 的控制
EXIT 用於強制結束整個 DO；CYCLE 用於跳過本次迭代，繼續下一次迭代。常見於條件過濾與倉促跳過的情境。

```fortran
program exit_cycle_demo
  implicit none
  integer :: i
  do i = 1, 10
     if (i == 5) exit            ! 直接結束整個 DO
     if (mod(i, 3) == 0) cycle     ! 避開本次迭代，繼續下一次
     print *, "i =", i
  end do
end program exit_cycle_demo
```

5) 嵌套迴圈與多維陣列
在需要填滿或處理二維資料時，可使用巢狀 DO：

```fortran
program grid_fill
  implicit none
  integer, parameter :: R = 3, C = 4
  integer :: r, c
  integer :: a(R, C)

  do r = 1, R
     do c = 1, C
        a(r, c) = r * c
     end do
  end do

  print *, "Array a:"
  do r = 1, R
     write(*,'(4(I5))') (a(r, c), c = 1, C)
  end do
end program grid_fill
```

6) 設計要點與最佳實務
- 盡量讓迴圈的開始終止條件清楚，避免不必要的空迭代，尤其當 end < start 或 step 的符號與方向不符時，記得提前檢查。  
- 若需要預先知道迭代次數，使用 n 的公式或先計算上限再進入迴圈，能提升執行效率並降低風險。  
- 對於長時間運算，善用 EXIT/CYCLE 與內部條件分支，避免在每次迭代中執行冗長判斷。  
- 若資料型別為實數，請注意步長計算的捨入問題，必要時用整數步長與分割區間的方法來確保穩定性。

結尾過渡：透過以上對 DO 迴圈與迭代控制的穩健寫法與實作範例，下一節我們將更深入探討嚴謹的輸入檢核與容錯機制，並開始模組化介面設計，為高階的資料處理與介面互動打下穩固的基礎。

### 節5：EXIT、CYCLE 與流程中斷

在長迴圈或多層巢狀迴圈中，適時地使用 EXIT 與 CYCLE，能讓程式在必要時提早結束或跳過不相關的迭代，提升效能並降低風險。EXIT 用於結束當前 DO 迴圈，CYCLE 則用於跳過本次迭代並繼續下一次。若有多層 DO，透過標籤（label）指定要退出或跳過的迴圈，能避免誤觸內層以致難以追蹤的邏輯錯誤。

在實作上，基本規則如下：
- 不帶標籤時，EXIT/CYCLE 作用於「最近的」DO。若要作用於上層，需使用帶標籤的形式。
- EXIT 用於「離開整個 DO」；CYCLE 用於「略過本次迭代，進入下一次迭代」。
- 嚴謹的巢狀結構，建議以命名的 DO（outer、inner）或數字標籤的方式，清楚定義要退出/跳過的層次。

為了讓概念更清楚，下面給出幾個實作範例與注意事項。

Code Block 1: 使用 EXIT 結束找到的條件
```fortran
! 範例1：在資料中尋找第一個超過閾值的元素，找到即退出
program find_threshold
  implicit none
  integer :: i, n
  real, dimension(100) :: arr
  real :: thr

  n = 100
  thr = 42.0
  do i = 1, n
     arr(i) = real(i) * 0.5
  end do

  do i = 1, n
     if (arr(i) > thr) then
        print *, 'Found at i =', i
        exit
     end if
     ! 其他處理
  end do
  print *, '結束搜尋'
end program find_threshold
```

Code Block 2: 使用 CYCLE 跳過不良資料
```fortran
! 範例2：遇到負值時跳過，專注有效資料的處理
program skip_negative
  implicit none
  integer :: i, n
  real, dimension(100) :: a

  n = 100
  do i = 1, n
     a(i) = real(i) - 60.0
  end do

  do i = 1, n
     if (a(i) .lt. 0.0) cycle  ! 跳過本次迭代，進入下一次
     ! 對有效資料進行密集計算
     call heavy_compute(a(i))
  end do
contains
  subroutine heavy_compute(x)
    real, intent(in) :: x
    ! 假設有大量運算
  end subroutine heavy_compute
end program skip_negative
```

Code Block 3: 兩層以上巢狀迴圈的「有條件退出/跳過」
```fortran
! 範例3：外層 DO 需在特定條件下終止
program nested_exit_cycle
  implicit none
  integer :: i, j
  integer, parameter :: M = 5, N = 6
  logical :: stop_all, skip_inner

  stop_all = .false.
  do i = 1, M
     skip_inner = .false.
     do j = 1, N
        if (some_condition(i, j)) cycle inner  ! 跳過本次 inner 的迭代
        if (stop_all_condition(i, j)) then
           exit outer  ! 退出到外層 DO
        end if
        ! 內層的大量運算
     end do
     outer: if (skip_inner) cycle  ! 某些情況可在外層控制跳過
  end do
end program nested_exit_cycle
```

補充說明與實用技巧
- 對於實數型資料的步長問題，避免步長在迭代中逐步累積造成舍入誤差，可以採用「以整數索引對應實值」的思路，或在每次迭代時先以整數索引推導出實值參數再進行運算。例如：
  - 若要以穩定步長進行區間分割，可改寫成若干整數步長的迴圈，並以公式重新計算要處理的實數位置，如 x_i = a + (i-1)Δx。內部的 x_i 可用公式重新計算，避免累積誤差的累積。
  - 整數步長的寫法範例：
  $$\text{do k = 1, Kmax}$$
  $$x_k = x_0 + (k-1)\Delta x$$
  如果 Δx 為實數，仍以每次 i 的運算式重新求得 x_k，而非長時間使用累加。
- 若必須依循 DO 的原生步長寫法，請確保 STEP 為整數，且望向前檢查條件，避免因舍入與越界造成未預期的結束。

在實務上，EXIT/CYCLE 能讓迴圈控制更直覺、可讀性更高，替代過度使用 GOTO 的情況，降低除錯成本與風險。多層迴圈時，記得用命名 DO 或標籤指定要退出/跳過的層次，以確保行為符合設計意圖。

結尾過渡：透過以上對 DO 迴圈與迭代控制的穩健寫法與實作範例，下一節我們將更深入探討嚴謹的輸入檢核與容錯機制，並開始模組化介面設計，為高階的資料處理與介面互動打下穩固的基礎。

## 第 2 章：章五：資料型態、變數與輸入輸出

### 節1：整數與實數的型別與範圍

在實務上，選對型別就像是為變數打上「適當尺寸的口罩」，既避免溢位又能提高效能與記憶體使用效率。本節先從整數與實數的基本型別與範圍說起，並說明如何用可攜性的方式指定其「等級（kind）」。

- 整數型別的範圍
  論及一般可攜性的整數，其實作與位元數有關。若以有號整數的位元數為 N，則其數值範圍大致為
  $$-2^{N-1} \le x \le 2^{N-1}-1.$$  
  為了跨平台穩定地指定整數的位元數，在 Fortran 中可以用 selected_int_kind 來取得適當的 kind，避免硬編碼位元數導致的不可攜性。例如：

  - ik32 至少具備 9 位十進位有效數字
  - ik64 至少具備 18 位十進位有效數字

  這樣的做法能讓我們宣告的整數在多種編譯器與平台上仍維持預期的範圍。

- 實數型別（單/雙精度）的範圍
  實數型別的範圍取決於浮點格式，一般而言單精度（四位元浮點，IEEE 754 32 位）與雙精度（IEEE 754 64 位）有下列常見範圍：
  - 單精度最大正值約為 $x_{\max} \approx 3.4028235\times 10^{38}$，最小正值約為 $x_{\min}^{+} \approx 1.175494\times 10^{-38}$。
  - 雙精度最大正值約為 $x_{\max} \approx 1.7976931\times 10^{308}$，最小正值約為 $x_{\min}^{+} \approx 2.2250739\times 10^{-308}$。
  為了跨平台穩定地宣告實數的精度與範圍，可以用 selected_real_kind(precision, range) 取得合適的 real kind，並以該 kind 建立變數。

- 字面量與型別後綴
  為了讓字面量與指定的 kind 一致，我們可以在常數後加入後綴，例如 0_ik32、1.0_rk32、1.0_ rk64 等等，這些後綴會讓常數自動符合所宣告的 kind。

- 輸出與範圍檢查的實作要點
  為了在程式中取得各自型別的極值與最小正值，可以利用 Fortran 的可用 Intrinsic 到位的函數：
  - 對整數使用 huge(x) 取得最大正整數，對最小值可用負的巨大值再減去 1。
  - 對實數使用 huge(x) 取得最大實數，tiny(x) 取得最小正規化實數。

以下為一個簡潔但可移植的實作範例，展示如何以 kind 的方式宣告，並列印各型別的極值與最小正值。

```fortran
program type_kinds_demo
  implicit none
  ! 選取可攜的整數種類
  integer, parameter :: ik32 = selected_int_kind(9)   ! 至少 9 位十進位
  integer, parameter :: ik64 = selected_int_kind(18)  ! 至少 18 位十進位

  ! 宣告整數範圍（透過 HUGE 的上限推得 min/max）
  integer(kind=ik32) :: i32_min, i32_max
  integer(kind=ik64) :: i64_min, i64_max

  ! 選取可攜的實數種類
  real, parameter :: rk32 = selected_real_kind(7, 37)   ! 至少 7 位十進位精度、範圍
  real, parameter :: rk64 = selected_real_kind(15, 307)

  real(kind=rk32) :: r32_minpos, r32_max
  real(kind=rk64) :: r64_minpos, r64_max

  ! 計算整數的極值
  i32_max = huge(0_ik32)               ! 最大整數
  i32_min = -huge(0_ik32) - 1           ! 最小整數
  i64_max = huge(0_ik64)
  i64_min = -huge(0_ik64) - 1

  ! 計算實數的極值與最小正值
  r32_max  = huge(0.0_rk32)
  r32_minpos = tiny(0.0_rk32)

  r64_max  = huge(0.0_rk64)
  r64_minpos = tiny(0.0_rk64)

  ! 輸出結果
  print *, "32-bit 整數範圍: ", i32_min, " ~ ", i32_max
  print *, "64-bit 整數範圍: ", i64_min, " ~ ", i64_max
  print *, "單精度實數範圍: 最大=", r32_max, ", 最小正值=", r32_minpos
  print *, "雙精度實數範圍: 最大=", r64_max, ", 最小正值=", r64_minpos
end program
```

說明
- 本範例使用可攜的 kind 推斷方式，避免直接寫死特定位元數，提升跨平台穩定性。
- 巨大值與最小正值的取得，透過 huge 與 tiny 與對應的常數型別後綴完成，並以 0_ik32、0_ik64、0.0_rk32、0.0_rk64 等形式指定常數型別。
- 如需更嚴格的自動檢查，可在實作中新增「自動檢查輸入範圍」的子程序，讓使用者輸入的值先經過範圍驗證再繼續處理。

結尾過渡：透過以上對整數與實數型別與範圍的穩健理解，下一節我們將更深入探討嚴謹的輸入檢核與容錯機制，並開始模組化介面設計，為高階的資料處理與介面互動打下穩固的基礎。

### 節2：布林、字元與字串

在 FORTRAN 的程式設計中，布林型和字元相關型別，是控制流程與文字處理的基礎。布林值用於條件判斷，字元與字串則承載文字資料與輸入輸出的介面。本節將說明布林值的表示與運算、單字元與字串的長度管理，以及常用的字串操作，並附上實作範例與可選的自動輸入範圍檢查子程序，幫助你建立穩健的文字與布林處理流程。

- 布林（Boolean）與邏輯運算
  - Fortran 的布林常數為 .TRUE. 與 .FALSE.，在控制流程與條件判斷時最常使用。
  - 邏輯運算符對應於 Fortran 為 .AND.、.OR.、.NOT.，對應的語法可寫成 A .AND. B、A .OR. B、.NOT. A，亦可用於 if 陳述式中。為了數學表示，布林運算在數學語彙可寫成 $A \\land B$、$A \\lor B$、$\\neg A$，而在程式中分別對應為 $A$ .AND. $B$、$A$ .OR. $B$、$\\neg A$。這樣的對照有助於設計條件結構與測試邏輯。  
$$  
A \land B = \begin{cases}
\text{TRUE}, & \text{若 } A = \text{TRUE 且 } B = \text{TRUE} \\
\text{FALSE}, & \text{其他情況}
\end{cases}
$$

對於布林值的比較，也可使用 A == B 的語法，與 floating 或整數的等值比較一致。
- 字元與字串
  - 單字元可用 character(len=1)，字串則是 character(len=長度) 的型別。字串的實作常需要處理長度與尾部空白，因此常用 len_trim(s) 取得去除尾端空白的實際長度，trim(s) 用於去除前後空白，adjustl(s) 可以左對齊字串。
  - 字串連接使用 //，取得子字串可用 s(i:j)， len(s) 及 len_trim(s) 提供字串長度的資訊。區分字串的實際內容長度與陳列長度，能讓輸出顯示更「整潔」，也便於自動檢查與格式化。

以下為實作範例，示範如何輸入布林、單字元與字串，以及基本的字串處理與輸出。

```fortran
! 範例：布林、字元與字串的輸入輸出與字串處理
program BoolCharStringDemo
  implicit none

  logical :: b
  character(len=1) :: ch
  character(len=100) :: s
  integer :: s_len
  character(len=20) :: tmp

  ! 輸入布林值 (TRUE / FALSE)
  print *, "請輸入布林值 (.TRUE. / .FALSE.):"
  do
     read *, tmp
     select case (trim(adjustl(tmp)))
     case ("TRUE", ".TRUE.")
        b = .TRUE.
        exit
     case ("FALSE", ".FALSE.")
        b = .FALSE.
        exit
     case default
        print *, "無效輸入，請重新輸入 TRUE 或 FALSE："
     end select
  end do
  if (b) then
     print *, "你輸入的布林為 TRUE"
  else
     print *, "你輸入的布林為 FALSE"
  end if

  ! 輸入單一字元
  print *, "請輸入單一字元 (任意字元):"
  read *, ch
  print *, "你輸入的字元是: ", ch(1:1)

  ! 輸入字串並進行基本處理
  print *, "請輸入字串 (最大長度100)："
  read *, s
  s_len = len_trim(s)
  print *, "字串實際長度（去尾空白後）: ", s_len
  print *, "去尾空白後字串: '", trim(s), "'"

  ! 示範連接與子字串
  print *, "連接範例：", trim(s) // " — 已輸入"
  if (s_len >= 4) then
     print *, "子字串(2..4): ", s(2:4)
  else
     print *, "字串長度不足以取(2..4)"
  end if
end program
```

若要讓輸入的長度自動落在某個範圍，亦可建立自動檢查子程序。以下為可選的模組化寫法，方便在多個地方重複使用：

```fortran
! 自動檢查輸入範圍的子程序：模組化設計
module input_checks
  implicit none
contains
  subroutine check_len(s, minlen, maxlen, ok)
    character(len=*), intent(in) :: s
    integer, intent(in) :: minlen, maxlen
    logical, intent(out) :: ok
    if (len_trim(s) >= minlen .and. len_trim(s) <= maxlen) then
       ok = .TRUE.
    else
       ok = .FALSE.
    end if
  end subroutine
end module
```

若以此模組檢查在主程序中使用，可在字串輸入後呼叫 check_len 並根據 ok 的值決定是否重新輸入，形成穩健的輸入驗證流程。

結尾過渡：透過以上對布林、字元與字串的穩健理解與基本操作，下一節我們將更深入探討嚴謹的輸入檢核與容錯機制，並開始模組化介面設計，為高階的資料處理與介面互動打下穩固的基礎。

### 節3：型別轉換與格式化輸出

在 FORTRAN 的世界裡，數值型別與輸入輸出格式是最常見的工作日常。本節聚焦兩大核心：型別轉換的基本用法與格式化輸出的實作要領，讓零基礎讀者也能在實作時掌握「何時轉換、用怎麼輸出」的穩健技巧。

- 型別轉換的基本觀念
  FORTRAN 提供顯式的型別轉換函數，如 real(x) 讓整數或其他型別轉成實數，int(y) 將實數轉成整數；此外還有 FLOOR、CEILING 兩個函數分別代表向下與向上取整。為避免自動型別推論導致的歧義，強烈建議在實作時使用 implicit none，並以明確的轉換符號控制資料型別與精度。

  轉換關係可用以下符號直觀表示：
  - $y = \mathrm{real}(x)$，其中 $x$ 為整數或其他整數型別，$y \in \mathbb{R}$。 
  - $z = \mathrm{int}(y)$，將實數轉成整數。注意可能產生小數部分被截斷的行為。
  - $f = \mathrm{FLOOR}(y)$ 與 $c = \mathrm{CEILING}(y)$ 分別對應無條件往下與往上取整。

  為了避免模糊，以下區分也很重要：INT 通常是把小數部分截斷（向零方向）、FLOOR 是向下取整、CEILING 是向上取整。若要更嚴謹地掌握轉換，建議以這些函數搭配使用，並針對負數情況做單元測試。

  為了視覺化地理解，以下是直觀的數學對照：
  $$
  y = \mathrm{real}(x), \quad z = \mathrm{int}(y), \quad f = \mathrm{FLOOR}(y), \quad c = \mathrm{CEILING}(y).
  $$
  此外，為了在程式中控制精度，常用的技巧是先用 KIND 指定實數的位元寬度，如 real( kind=selected_real_kind(12, 300))，以確保在不同平台的一致性。

- 格式化輸出的核心要領
  輸出時，我們用寫入語句 (write) 搭配格式字串，或使用內部格式字串（internal file）把輸出先放到字串中，再顯示。常見的格式機制包括整數 In、浮點數 Fm.n、科學記號 En，以及字串 A。格式字串的語法，是在括弧內以字元序列定義寬度與精度，如 '(I6, F8.2, A)'。

  常見的練習要點：
  - 以外部檔輸出，確定欄位寬度與小數點位數，避免資料被截斷。
  - 以內部字串輸出，便於再加工或拼接輸出結果。
  - 配合 implicit none，提高輸出內容的穩健性與可讀性。

- 程式範例一：基本型別轉換與格式化輸出
  下面的程式示範：宣告、基本轉換、以及以格式字串輸出。內含註解說明，便於初學者理解。

```fortran
program type_conversion_output
  implicit none
  integer :: i
  real    :: r
  character(len=20) :: s
  integer :: n
  real    :: x
  character(len=60) :: out

  i = 7
  r = real(i)                 ! 整數轉實數
  s = '42'
  read(s, *) n                 ! 字串轉整數
  x = 3.14159
  i = int(x)                    ! 實數轉整數（截斷）

  write(*, '(A, I6, A, F8.3)') '轉換結果: i=', i, ', r=', r
  write(out, '(A, I3, A, F6.2)') '內部字串格式化: ', n, ' -> ', x
  print *, trim(out)
end program
```

- 程式範例二：字串與數值的互轉與容錯輸入
  本例展示如何從字串中讀取數值，以及如何用內部格式先行驗證，提升輸入穩健性。

```fortran
program string_to_number
  implicit none
  character(len=30) :: line
  integer :: iv
  real    :: rv

  line = '  128  '
  read(line, '(I3)') iv          ! 字串到整數的轉換
  line = '  3.1415 '
  read(line, '(F6.3)') rv         ! 字串到實數的轉換

  write(*, '(A, I6, A, F6.3)') 'iv=', iv, ' rv=', rv
end program
```

- 結尾過渡
  透過以上對型別轉換與格式化輸出的基礎理解與實作，讀者已具備在不同情境下穩健輸入與清晰輸出之能力。下一節我們將進一步把這些技術落實於嚴謹的輸入檢核與容錯機制，並開始模組化介面設計，為高階的資料處理與介面互動打下穩固的基礎。

### 節4：輸入資料與錯誤處理

在前一節中，我們已練習了資料型態與格式化輸出，接著要把「輸入」從使用者端穩健取得，並在遇到異常時給予友善的回饋與適當的處理。輸入錯誤常見原因包括型別不符、數值超出範圍、資料截斷與檔案結束（EOF）。透過 IOSTAT、ERR、END 等機制，我們可以在不中斷程式邏輯的情況下，做出清晰的錯誤分支與回復。

1) 基本觀念：IOSTAT、ERR、END
- 讀取時若成功，IOSTAT 會等於 0；若發生錯誤，IOSTAT 為非 0 的整數值。Inline 表示為 $IOSTAT = 0$ 表示成功，$IOSTAT \neq 0$ 表示發生錯誤。
- READ 指定 IOSTAT 與 END/ERR 分支可讓你在錯誤情況下轉移到對應的處理程式區塊。區塊屬性範例如下：
  - $$\text{若 ID 讀取成功，繼續執行；若 } IOSTAT \neq 0 \text{，執行錯誤處理}$$
  - END 指定在遇到檔案結束時的跳轉，ERR 指定在遇到格式錯誤、非法輸入時的跳轉。

2) 基本範例：單次讀取與簡易錯誤處理
- 這段程式展示如何用 List-directed 輸入與 IOSTAT 監控輸入品質，若出現錯誤，先輸出訊息再結束或重試。
```fortran
program simple_input
  implicit none
  integer :: iv
  real    :: rv
  integer :: ios

  print *, '請輸入整數 iv 與實數 rv，用空白分隔：'
  read(*, *, iostat=ios) iv, rv
  if (ios /= 0) then
     print *, '輸入錯誤，iostat = ', ios
     stop
  else
     print '(A, I6, A, F6.3)', 'iv=', iv, ' rv=', rv
  end if
end program
```
- 這裡的要點是：立即查詢 IOSTAT，若不為 0，代表輸入格式不符合需求；適度地讓使用者知道錯誤、再決定結束或重試。

3) 進階做法：以輸入列為單位的穩健重試
- 為了提升友善度與穩健性，可以使用「先讀整行再進行內部解析」的策略，將輸入限制與格式檢核分離，便於告知使用者錯誤內容。
```fortran
program robust_input
  implicit none
  integer :: iv
  real    :: rv
  integer :: ios
  character(len=128) :: line

  do
     print *, '請輸入整數 iv 與實數 rv（如：12 3.45），或取消輸入：'
     read(*,'(A)') line
     ! 嘗試以內部檔案解析
     read(line, *, iostat=ios) iv, rv
     if (ios == 0) exit
     print *, '格式錯誤或無法解析，請再試一次。'
  end do

  print *, '成功讀入: iv =', iv, ' rv =', rv
end program
```
- 這個模式提升了容錯性：即使使用者先前輸入不完整或格式錯誤，程式仍能要求使用者重新輸入，而不會直接崩潰。

4) 輸入型態與範圍檢核
- 對於數值型資料，除了型別正確外，常需再做範圍檢查，例如年齡、分數、百分比等，避免極端值造成後續演算法失效。示例：
```fortran
if (ios == 0) then
   if (iv < 0 .or. iv > 120) then
      print *, '錯誤：年齡 iv 超出合理範圍 [0, 120]。'
      stop
   endif
   if (rv < 0.0) then
      print *, '錯誤：實數 rv 不可為負值。'
      stop
   endif
else
   print *, '輸入格式錯誤，請重新輸入。'
end if
```
- 對字串輸入，常需修剪尾端空白與驗證長度，避免造成緩衝溢出。Fortran 提供 TRIM、ADJUSTL 等內建函式協助處理。

5) 錯誤狀況的系統化處理
- 建立穩健的輸入介面模組，讓不同模組或子程式可以重複使用相同的輸入檢核邏輯。原則如下：
  - 預期輸入型別與格式清楚定義。
  - 以 IOSTAT、END、ERR 進行分支，避免硬編碼跳轉。
  - 預設提供使用者重試機制，避免任意非預期狀況直接終止。
  - 對不可修復的錯誤，提供清楚的錯誤訊息與結束條件。

6) 常見陷阱與解法
- 容易忽略的緩衝區問題：讀取失敗後需清空緩衝，否則後續讀取仍可能受到前一次輸入的干擾。
- EOF 處理：若讀到結尾，要有適當的結束邏輯或給出明確訊息，而非陷入死循環。
- 格式化差異：自由格式與定義格式混用時，保留一致的解析策略，避免混淆。

結尾過渡
透過以上對輸入與錯誤處理的實作與檢核，讀者已具備在各種情境下穩健取得輸入與清晰回應的能力。下一節我們將進一步把這些技術落實於嚴謹的輸入檢核與容錯機制，並開始模組化介面設計，為高階的資料處理與介面互動打下穩固的基礎。

### 節5：多變數與格式化讀取

本節聚焦在同一條輸入線上同時取得多個變數，以及自由格式與定義格式混用時的穩健解析策略。為避免前文提到的一次輸入干擾，我們透過清晰的分段與退出機制，讓程式遇到 EOF 或格式錯誤時能有清楚的結束與訊息。

- 自由格式讀取與多變數
  - 以 READ(*,*) 進行 List-Directed 輸入時，數字、字串等會以空白分隔，資料可以跨行成長。若輸入未填完整個變數清單，下一輪讀取會在下一行繼續補齊。
  - EOF 或連續讀取結束時，必須設計結束邏輯，避免死循環。通常會使用END=標籤，或 IOSTAT 檢查來判斷結束與錯誤情形。

- 定義格式讀取與混用策略
  - 使用格式字串（例如 '(I5,F8.2,A3)'）的定義格式，能嚴格控制每個欄位的寬度與類型，適合固定寬度的資料來源。
  - 混用時，建議保持一致的解析策略：若大量資料是自由格式，就以自由格式為主；若資料來自固定寬度檔案，則以定義格式為主，偶爾再轉換。
  - 可同時支援兩種模式，但在同一段輸入區塊中應避免頻繁切換，以免造成可讀性與除錯困難。

- EOF 與錯誤的穩健處理
  - END= label：當遇到檔尾時，程式會跳轉到指定的標籤，讓使用者得到明確的結束訊息。
  - IOSTAT：將 IOSTAT 變數檢查為非零時，代表讀取發生問題（如格式不符、 EOF 以外的錯誤）。需搭配適當的分支處理。
  - 提供明確回饋，例如輸出“EOF 已到達，共讀取 X 筆紀錄”或“讀取失敗，stat=…”。

- 進階技巧：讀取陣列與同構欄位
  - 直接一次性讀取整個陣列，適合每列含同類型的多筆數值。
  - 使用 (a(i), i=1,n) 的 implied do，可在單次語句中完成多個變數的填充。

程式範例
- 自由格式多變數讀取（EOF 使用 END 標籤）
```fortran
program multi_free
  implicit none
  integer :: a, b
  real    :: c
  integer :: count

  count = 0
  do
     read(*,*,END=99) a, b, c
     count = count + 1
     print *, 'Record', count, ': ', a, ', ', b, ', ', c
  end do
99    continue
  print *, 'EOF reached after', count, 'records'
end program multi_free
```

- 定義格式讀取與混用策略（範例：I5, F8.2, A3）
```fortran
program multi_formatted
  implicit none
  integer :: i
  real    :: x
  character(len=3) :: tag
  integer :: count

  count = 0
  do
     read(*,'(I5,F8.2,A3)',END=99) i, x, tag
     count = count + 1
     print *, 'Record', count, ': ', i, ', ', x, ', ', trim(tag)
  end do
99    continue
  print *, 'EOF reached after', count, 'records'
end program multi_formatted
```

- 陣列一次性讀取與 IOSTAT 檢查
```fortran
program read_array
  implicit none
  integer, parameter :: n = 4
  real :: arr(n)
  integer :: stat
  integer :: i

  ! 以自由格式一次性讀取整個陣列
  read(*,*,IOSTAT=stat) (arr(i), i=1,n)
  if (stat == 0) then
     print *, 'arr = ', (arr(i), i=1,n)
  else
     print *, '資料不足或格式錯誤，stat = ', stat
  end if
end program read_array
```

- 混用情境（自由格式主，定義格式作局部控制的示範）
```fortran
program mixed_input
  implicit none
  integer :: a
  real    :: v
  character(len=5) :: lab
  integer :: stat

  ! 自由格式讀入，一次取三個資訊
  read(*,*,END=99) a, v, lab
  print *, '自由格式讀取 =>', a, v, trim(lab)

  ! 若需要更嚴格的欄位控制，可改用定義格式
  read(*,'(I5,F8.2,A5)',END=99) a, v, lab
  print *, '定義格式讀取 =>', a, v, trim(lab)
99 continue
  print *, 'EOF 或結束狀況已處理完畢'
end program mixed_input
```

結尾過渡
透過以上對多變數與格式化讀取的實作與檢核，讀者已具備在不同資料來源與情境下穩健取得輸入並清晰回應的能力。下一節我們將進一步把這些技術落實於嚴謹的輸入檢核與容錯機制，並開始模組化介面設計，為高階的資料處理與介面互動打下穩固的基礎。

## 第 3 章：章六：陣列、子程式與模組

### 節1：一維與多維陣列的建立

在 Fortran 的學習旅程中，陣列是最核心的資料結構之一。無論是處理向量、矩陣，抑或是多維張量，一次建立好適當的形狀與下界，後續的運算、切片與分配都能更直接、清晰地寫出來。本節聚焦於如何建立一維與多維陣列，包含靜態宣告與動態分配，以及自訂下界的技巧。

- 一維陣列的建立
  - 靜態宣告與初始化（預設下界為 1）
    Fortran 的基本宣告可直接指定長度與型別，並給予初值。常見寫法如下：
    Code Block:
    ```fortran
    program one_d_static
      implicit none
      integer, dimension(5) :: a       ! 靜態一維陣列，長度為 5
      a = (/ 1, 2, 3, 4, 5 /)          ! 使用陣列構造子初始化
      print *, "a =", a
    end program one_d_static
    ```
  - 自訂起始下界（例如從 0 開始）
    Fortran 允許為每個維度指定下界，常見寫法是 dimension(0:4) 表示總數仍然是 5，但索引從 0 開始。
    Code Block:
    ```fortran
    program one_d_lowerbound
      implicit none
      integer, dimension(0:4) :: b = (/ 0, 1, 2, 3, 4 /)
      print *, "b(0)=", b(0), " b(4)=", b(4)
    end program one_d_lowerbound
    ```
  - 動態分配的一維陣列
    使用 allocatable 與 allocate，可以在執行階段決定長度，並可在結束後 deallocate 釋放記憶體。
    Code Block:
    ```fortran
    program one_d_alloc
      implicit none
      integer, allocatable :: c(:)
      integer :: i
      allocate(c(10))
      do i = 1, 10
        c(i) = i*i
      end do
      print *, "c =", c
      deallocate(c)
    end program one_d_alloc
    ```
  - 使用 array constructor 直接分配與初始化
    如果已知元素數量，可用陣列構造子快速建立並初始化。
    Code Block:
    ```fortran
    program one_d_constructor
      implicit none
      integer, dimension(5) :: d
      d = [(i, i=1,5)]
      print *, "d =", d
    end program one_d_constructor
    ```

- 多維陣列的建立
  - 靜態宣告的基本寫法
    常見是先宣告型別，指定每個維度的範圍。In Fortran，右邊的索引變化最快，因此 A(i,j) 中 i 是第一維度，j 是第二維度，記憶體走向為列主序。
    Code Block:
    ```fortran
    program two_d_static
      implicit none
      real, dimension(3,4) :: A      ! 3x4 的二維陣列
      A = 0.0
      A(1,1) = 1.0
      A(2,3) = 2.5
      print *, "A(2,3)=", A(2,3)
    end program two_d_static
    ```
  - 動態分配的多維陣列
    使用 allocatable 搭配 allocate，亦可為各維度設定自訂範圍，如同一維支援 0 起始。
    Code Block:
    ```fortran
    program two_d_alloc
      implicit none
      real, allocatable :: B(:,:)
      integer :: i, j
      allocate(B(0:2, 0:3))  ! 3x4 的陣列，第一維 0..2，第二維 0..3
      B = 0.0
      do j = 0, 3
        do i = 0, 2
          B(i,j) = real(i + j)
        end do
      end do
      print *, "B(2,3)=", B(2,3)
      deallocate(B)
    end program two_d_alloc
    ```
  - 多維與記憶體佈局的要點
    Fortran 採用列主序（右邊的索引變化最快），若有 shape( n1, n2, …, nk )，元素數量為 $\prod_{t=1}^k n_t$，且線性索引與子腳本的對應關係可寫成 $L = i_1 + (i_2-1) \cdot n_1 + (i_3-1) \cdot (n_1 n_2) + \cdots$。其中 $i_t$ 為第 t 維度的下標，滿足 $1 \le i_t \le n_t$。
    式中若以固定維度 $n_1$ 作為第一維長度，則簡化版可以寫成 $L = i + (j-1)\,n_1$（對兩維情況）。這有助於理解在非連續寫法或手動計算索引時的對應關係。

- 陣列操作與常用工具
  - 取得維度大小與元素個數：size(A, dim) 與 size(A)
  - 陣列切片與子陣列：如 A(2:4, :)、B( :, 1:2 )
  - 陣列與整數混合運算、自動向量化的特性，能讓整段運算以向量化方式執行，提升效能。

結語與過渡
透過上述結構與範例，讀者已能在靜態與動態情境下建立一維與多維陣列，並理解其下界設定與記憶體佈局。下一節將把這些陣列能力整合到實作層面的輸入檢核與容錯設計，並引入模組化介面，為高階的資料處理與介面互動奠定穩固基礎。

### 節2：子程式與函數的基本用法

在 FORTRAN 中，子程式與函數是封裝重複運算的核心工具。子程式透過 call 呼叫，通常沒有直接返回值；函數則在呼叫處給出一個值，能寫在表達式中使用。與前一節的陣列工作搭配，適時把重複的運算封裝成小模組，讓整段程式更具可閱讀性與可維護性。

- 基本語法要點
  - 子程式（subroutine）範例
  - 函數（function）範例
  - 透過 intent 指定參數的輸入/輸出行為
  - 模組化安排，透過 use 取得介面

以下為常見的實作範例，使用 explicit none、模組化組織與向量化友善的寫法。

```fortran
! util.f90
module util
  implicit none
contains
  ! 函數：回傳兩個整數中的較大者
  integer function max2(a, b)
    integer, intent(in) :: a, b
    if (a > b) then
      max2 = a
    else
      max2 = b
    end if
  end function max2

  ! 子程式：交換兩個整數的值
  subroutine swap(a, b)
    integer, intent(inout) :: a, b
    integer :: tmp
    tmp = a
    a = b
    b = tmp
  end subroutine swap

  ! 另一個子程式：陣列相加
  subroutine add_arrays(n, x, y, z)
    integer, intent(in) :: n
    real, intent(in)  :: x(n), y(n)
    real, intent(out) :: z(n)
    z = x + y        ! 向量化運算，自動對應每個元素
  end subroutine add_arrays
end module util
```

```fortran
! main.f90
program demo_sub_and_func
  use util
  implicit none

  integer :: a, b, m
  real, dimension(4) :: A, B, C

  A = (/ 1.0, 2.0, 3.0, 4.0 /)
  B = (/ 0.5, 0.5, 0.5, 0.5 /)

  ! 使用函數求較大值
  a = 7
  b = 4
  m = max2(a, b)
  print *, "max2(", a, ",", b, ") = ", m

  ! 使用子程序交換兩值
  call swap(a, b)
  print *, "after swap: ", a, b

  ! 使用向量化陣列加法
  call add_arrays(4, A, B, C)
  print *, "C = ", C
end program
```

- 傳參與資料流向的重點
  - 參數預設以「引用傳遞」（pass-by-reference）機制傳遞，這也是為什麼 subroutine 會直接改變呼叫端的變數。若不希望修改，需在參數宣告上用 intent(in) 指定為唯讀。
  - 對於需回傳新值的情況，函數透過返回值提供結果；若要同時產出多個結果，通常以多個資料參數作為輸出，搭配適當的 intent 設定。

- 與向量化與陣列的結合
  - 在上面的 add_arrays 範例中，z = x + y 展現 Fortran 的陣列運算特性，能對整個陣列同時進行運算，效率通常優於逐元素迴圈。這也是本章前半段提到的「自動向量化」概念的實作表現之一。
  - 若要對多維陣列或子陣列做操作，只要參數型別、形狀與界限正確，FORTRAN 會自動對應每個元素。

- 模組化與介面的小提醒
  - 將常用的函數與子程式放在模組中，方便在其他程式中重複使用，且能透過 use 取得公開介面。
  - 對於較複雜的使用情境，例如需要把函數作為參數傳遞給另一個子程式進行回調，則需引入更完整的介面與抽象介面（abstract interfaces）與實作，這在之後的章節會逐步講解。

結語與過渡
透過本節的範例，你已掌握如何在靜態與動態情境下，以子程式與函數封裝重複的運算，並善用 intent 與模組化介面提升穩定性。下一節將把這些元件與輸入檢核與容錯設計結合，介紹如何在實作層面建立健全的介面與錯誤處理策略，為高階的資料處理與介面互動奠定穩固基礎。

### 節3：模組與模組程序的使用

模組（module）是 Fortran 的核心封裝單位，可把資料與可重用的子程式集中管理，並以 use 引入到需要的程式中。透過模組，可以控制可見性（private/public），讓介面穩定且易於維護。此外，模組程序（module procedures）與泛型介面（generic interface）讓相同名稱可以對應不同型別的實作，提升重用性與可讀性。

- 何時使用模組：當多個程式單元需要共享演算法、資料型態或常數時，使用模組可避免重複定義，並確保一致性。
- 泛型介面與模組程序：以 interface ... end interface 定義泛型入口，將不同型別的實作綁在同一個名字下，呼叫端不需關心底層型別。

以下範例展示如何定義一個整數與實數版本皆能呼叫的加法泛型介面，以及對應的模組程序與使用方法。

```fortran
module numbers
  implicit none
  private
  public :: add
  interface add
     module procedure add_int
     module procedure add_real
  end interface
contains
  function add_int(a,b) result(res)
    integer, intent(in) :: a,b
    integer :: res
    res = a + b
  end function add_int

  function add_real(a,b) result(res)
    real, intent(in) :: a,b
    real :: res
    res = a + b
  end function add_real
end module numbers
```

```fortran
program test_numbers
  use numbers
  implicit none
  integer :: iv
  real :: rv

  iv = add(2, 3)          ! 呼叫整數版本
  rv = add(1.5, 2.5)       ! 呼叫實數版本

  print *, 'iv =', iv
  print *, 'rv =', rv
end program test_numbers
```

上例中，模組 numbers 使用 private 來隱藏內部實作，只有 add 介面對外公開；透過 interface 區塊與 module procedure 的連結，呼叫 add 時會自動判別參數型別並選取對應的實作，符號層級保持統一但型別分派在編譯時完成。若需要支援多型或動態選擇，則可在模組中再引入抽象介面與流程控制。

為了未來的回呼機制與高階介面，本文也先提及抽象介面（abstract interfaces）的概念與實作架構，雖在本節僅作初步示範，實際用法會在後續章節逐步展開。以下是概念性範例，顯示如何定義可被指派為回呼的介面雛形：

```fortran
module abstract_demo
  implicit none
  private
  public :: cb_holder, set_cb, call_cb

  abstract interface
     function cb(x) result(y)
       integer, intent(in) :: x
       integer :: y
     end function cb
  end interface

  type :: cb_holder
     procedure(cb), pointer :: f => null()
  end type cb_holder

contains
  function call_cb(h, v) result(res)
    type(cb_holder), intent(in) :: h
    integer, intent(in) :: v
    integer :: res
    if (associated(h%f)) then
       res = h%f(v)
    else
       res = -1
    end if
  end function call_cb
end module abstract_demo
```

此段示意抽象介面與回呼結構的基本骨架，實作細節與安全性檢查將在後續章節實作中逐步講解。

結語與過渡
透過本節的範例，你已掌握如何在靜態與動態情境下，以模組與模組程序封裝重複運算，並善用泛型介面提升穩定性與可讀性。接下來，將把這些元件與輸入檢核與容錯設計結合，介紹在實作層面建立健全的介面與錯誤處理策略，為高階的資料處理與介面互動奠定穩固基礎。

### 節4：呼叫約定與參數傳遞

在前一節的抽象介面與回呼結構之後，本節聚焦實務層面的「呼叫約定」與「參數傳遞」細節，讓你清楚瞭解 Fortran 在模組化程式設計中如何把外部函式作為參數傳入、以及如何用直覺的語法保護參數的生命週期與型別安全。核心觀念集中在三件事：參數傳遞的語意、實參與虛參的對應、以及如何用 explicit interface 與 optional/intent 等機制提升穩定性與可讀性。為了讓說明貼近實作，我們以本書前文的 d module abstract_demo 為例，展示一個高階函式介面的設計與使用方式。

重點整理
- 呼叫約定的本質：Fortran 的大多數實參都是以「參考傳遞」方式進行，即實參與 dummy 參數在記憶體位址上共用同一個位置。若 dummy 的意圖是輸入或輸出，必須透過 intent(in)、intent(out) 或 intent(inout) 來標示，Compiler 會在編譯期檢查不合法的寫入。因此，若 a 是陣列或變數，實際上會指向同一個儲存區，除非語意改變（如新值被回寫）。對應公式可寫成：$a_i' = f(a_i) \quad (i=1,\ldots,N)$ 表示對每個元素經由傳入的函式改寫，實際參數仍指向同一塊記憶體。  
- explicit interface 與 callback：若要把函式作為參數傳入，必須給出該被呼叫的函式的介面，避免型別與輸入輸出張力。介面可使用 abstract interface 定義一個「一元函式」的型別；dummy argument 則用 procedure(...) 來宣告。這樣編譯器就能在呼叫時檢查是否符合介面的簽章，避免傳入錯誤型別的子程式。
- optional 與 present 檢查：實參可以是選擇性的，並以 present() 來檢查是否提供了該參數，語法與語意清晰，提升容錯性。若未提供，可使用預設路徑或提供替代函式。
- 命名實參與類型安全：Fortran 支援命名實參（named actual arguments），在多參數呼叫時能提高可讀性與維護性。搭配 explicit interface，可在編譯期就抓到參數錯置的問題。

以下為實作範例，分成模組與外部呼叫兩部分，皆著重於「呼叫約定與參數傳遞」的實作細節。

Code Block 1: 模組 d 模組 abstract_demo（定義抽象介面與高階函式）
```fortran
module abstract_demo
  implicit none
  private
  public :: map_inplace, apply_to_array
  ! 抽象介面：定義一元函式的介面
  abstract interface
     function unary_op(x) result(y)
       real, intent(in) :: x
       real :: y
     end function unary_op
  end interface
contains
  ! 將傳入的一元函式應用於陣列元素，結果回寫至同樣大小的陣列
  subroutine map_inplace(n, a, op)
     integer, intent(in) :: n
     real, intent(inout) :: a(n)
     procedure(unary_op) :: op
     integer :: i
     do i = 1, n
        a(i) = op(a(i))
     end do
  end subroutine map_inplace

  ! 另一個範例：把來源陣列經函式轉換後放到目的陣列
  subroutine apply_to_array(n, src, dst, op)
     integer, intent(in) :: n
     real, intent(in) :: src(n)
     real, intent(out) :: dst(n)
     procedure(unary_op) :: op
     integer :: i
     do i = 1, n
        dst(i) = op(src(i))
     end do
  end subroutine apply_to_array
end module abstract_demo
```

Code Block 2: 使用範例程式（演示如何傳入實作的函式）
```fortran
program test_callback
  use abstract_demo
  implicit none
  integer :: n
  real, allocatable :: arr(:)
  n = 6
  allocate(arr(n))
  arr = [(real(i), i=1,n)]
  ! 把 square 作為回呼函式傳入
  call map_inplace(n, arr, square)
  print *, "arr after square:", arr
contains
  ! 一個符合 unary_op 介面的實作：平方函式
  function square(x) result(y)
     real, intent(in) :: x
     real :: y
     y = x*x
  end function square
end program test_callback
```

補充說明與實務注意
- 對於函式作為參數，確保被呼叫的函式具備與介面相容的簽章；若存在多個可能的簽章，方便透過模組化介面或泛型介面清晰定義，避免模組外部呼叫時的型別歧義。
- 若想要讓呼叫更靈活，可以在 map_inplace 中加入 optional 參數與 present() 檢查，例如選用不同的回呼函式：當 present(op) 時使用該函式，否則使用預設的單純函式。
- 為了可讀性與穩定性，建議在實作中始終使用 explicit interfaces，並將模組中所有相關的子程式與介面清楚公開，以避免呼叫端的誤用。
- 關於呼叫成本與效能，Fortran 的參考傳遞通常不影響效能，只有在函式內部改動實參的內容才會造成副作用；透過 intent 與可選參數，能讓編譯器做更好的優化。

結語與過渡
透過本節的範例，你已掌握如何在靜態與動態情境下，以模組與模組程序封裝重複運算，並善用泛型介面提升穩定性與可讀性。接下來，將把這些元件與輸入檢核與容錯設計結合，介紹在實作層面建立健全的介面與錯誤處理策略，為高階的資料處理與介面互動奠定穩固基礎。

### 節5：範例：小型計算模組的設計

本節透過一個「計算工具模組」的設計與實作，說明如何以模組與模組程序封裝重複運算，並善用泛型介面提升穩定性與可讀性。核心思路是把常用的算術運算與向量運算放在模組中，對外提供清晰的介面與型別安全的實作，讓上層應用只需呼叫介面，而不必關心細部實作。值得注意的是，參數傳遞採用預期傳遞與剛性型別檢查，能讓編譯器進行更好的優化，副作用更易控制。

- 設計要點
  - 封裝與介面穩定：把核心運算封裝成模組程序，對外暴露穩定的介面，避免外部直接操作內部狀態。
  - 泛型介面：使用泛型介面（generic interface）讓不同型別的實作共用同一名稱，如 add(real) 與 add(integer) 可透過同名介面呼叫不同實作。
  - 安全與容錯：在向量運算中檢查長度、提供簡單的錯誤訊息，避免未定義存取；在需要時提供可選參數與預設值以降低錯誤風險。
  - 動靜態情境並用：同時支援靜態型別的固定介面與動態情境下的容錯檢核，提升模組的可重用性。

- 公式提醒
  - 向量內積定義：  
    $$ x \cdot y = \sum_{i=1}^n x_i y_i $$
  - 歐幾里得長度：  
    $$ ||x||_2 = \sqrt{ \sum_{i=1}^n x_i^2 } $$

- 範例程式碼：模組 calc_mod
```fortran
module calc_mod
  implicit none
  private
  public :: add, norm2, dot, scale_vec

  interface add
     module procedure add_real
     module procedure add_int
  end interface

contains
  ! 加法：實數與整數各自的實作
  pure function add_real(a,b) result(res)
     real(kind=real64), intent(in) :: a,b
     real(kind=real64) :: res
     res = a + b
  end function add_real

  pure function add_int(a,b) result(res)
     integer, intent(in) :: a,b
     integer :: res
     res = a + b
  end function add_int

  ! 向量點積：對 real 向量的實作
  pure function dot(n, x, y) result(res)
     integer, intent(in) :: n
     real(kind=real64), intent(in) :: x(n), y(n)
     real(kind=real64) :: res
     integer :: i
     res = 0.0_real64
     if (n > 0) then
        do i = 1, n
           res = res + x(i) * y(i)
        end do
     end if
  end function dot

  ! 模組介面宣告 dot 的實作
  interface dot
     module procedure dot
  end interface

  ! 步長向量縮放，使用可選參數 alpha
  pure subroutine scale_vec(n, x, y, alpha)
     integer, intent(in) :: n
     real(kind=real64), intent(in) :: x(n)
     real(kind=real64), intent(out) :: y(n)
     real(kind=real64), optional, intent(in) :: alpha
     real(kind=real64) :: s
     if (present(alpha)) then
        s = alpha
     else
        s = 1.0_real64
     end if
     y = s * x
  end subroutine scale_vec

  ! 簡單的歸一化示例（如需暴露可用介面，可再包裹 norm2）
  pure function norm2(n, x) result(res)
     integer, intent(in) :: n
     real(kind=real64), intent(in) :: x(n)
     real(kind=real64) :: res
     res = sqrt( sum( x(1:n)**2 ) )
  end function norm2
end module calc_mod
```

- 範例使用說明（測試程式）
```fortran
program test_calc
  use iso_fortran_env, only: real64
  use calc_mod
  implicit none

  real(kind=real64) :: a,b, r
  integer :: ia, ib, ri
  real(kind=real64), dimension(3) :: x, y
  real(kind=real64) :: nx
  integer :: n

  a = 3.0_real64; b = 4.0_real64
  r = add(a,b)
  print *, "add real: ", r

  ia = 2; ib = 5
  ri = add(ia, ib)
  print *, "add int: ", ri

  n = 3
  x = [ 1.0_real64, 2.0_real64, 3.0_real64 ]
  y = [ -1.0_real64, 0.0_real64, 4.0_real64 ]
  r = dot(n, x, y)
  print *, "dot: ", r

  nx = norm2(n, x)
  print *, "norm2: ", nx
end program test_calc
```

- 動態情境與容錯設計說明
  - 可選參數的引入使模組在保持介面穩定的同時，能依需求提供更多樣的功能，例如 scale_vec 允許選擇縮放因子而不改變呼叫端介面。
  - 對於向量運算，加入長度檢查與錯誤訊息，若 size(x) 或 size(y) 不符時，回傳預設值並輸出警告，避免未定義的存取與難以追蹤的錯誤。
  - 泛型介面帶來的好處是，上層程式不需改動就能支援其他數值型別，只要在模組中補充對應的實作即可，提升穩定性與可讀性。

- 小結與過渡
 透過上述設計，你已掌握以模組封裝重複運算的思路，並在靜態與動態情境下，透過泛型介面與模組程序實作穩健的小型計算模組。接下來，將把這些元件與輸入檢核與容錯設計結合，介紹在實作層建立健全的介面與錯誤處理策略，為高階的資料處理與介面互動奠定穩固基礎。

# 第 3 篇：第三篇：實作與最佳實踐

## 第 1 章：章七：小型科學計算案例：向量與矩陣運算

### 節1：向量和矩陣的基本運算

在零基礎的 FORTRAN 教學中，向量與矩陣的基本運算是建立後續模組設計的核心。先把資料型態與索引邊界清楚定義，再以泛型介面包裝常用演算，便能在不同數值型別間重複使用，而不影響上層程式結構。向量通常是一維陣列，矩陣是二維陣列；當下限設定為 0 時，可清楚呈現「向左與向下的起點皆為 0」的零基礎觀念。基本運算包含：向量相加與標準化、向量點積與長度、矩陣-向量乘法、以及矩陣-矩陣乘法。下列公式說明直觀概念。

- 向量相加：$\text{若} v, w ∈ R^n，(v + w)_i = v_i + w_i，i = 0,…,n-1$
- 向量長度：$||v|| = sqrt(∑_{i=0}^{n-1} v_i^2)$
- 矩陣-向量乘法：$y = A x，y_i = ∑_{j=0}^{m-1} A_{i j} x_j$
- 矩陣-矩陣乘法：$C = A B，C_{i k} = ∑_{j=0}^{p-1} A_{i j} B_{j k}$

以下以模組化設計呈現，重點在於把運算內容與型別無關的介面包裝好，並以 0 起點的陣列實作實務，方便日後擴充為複數或自訂型別的情況。

```fortran
module vecops
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  private
  public :: vec_add, vec_dot, matvec, matmul2d
  interface vec_add
     module procedure vec_add_real
     module procedure vec_add_complex
  end interface
  interface vec_dot
     module procedure vec_dot_real
     module procedure vec_dot_complex
  end interface

contains

  ! 1) 向量相加 (實數)
  subroutine vec_add_real(a, b, c)
    real(real64), intent(in)  :: a(:), b(:)
    real(real64), intent(out) :: c(:)
    c = a + b
  end subroutine

  ! 2) 向量相加 (複數) - 泛型介面示例
  subroutine vec_add_complex(a, b, c)
    complex(real64), intent(in)  :: a(:), b(:)
    complex(real64), intent(out) :: c(:)
    c = a + b
  end subroutine

  ! 3) 向量點積 (實數)
  function vec_dot_real(a, b) result(res)
    real(real64), intent(in) :: a(:), b(:)
    real(real64)             :: res
    res = dot_product(a, b)
  end function vec_dot_real

  ! 4) 向量點積 (複數)
  function vec_dot_complex(a, b) result(res)
    complex(real64), intent(in) :: a(:), b(:)
    complex(real64)             :: res
    res = dot_product(a, b)
  end function vec_dot_complex

  ! 5) 矩陣-向量乘法 (實數)
  subroutine matvec(A, x, y, n, m)
    real(real64), intent(in)  :: A(0:n-1, 0:m-1)
    real(real64), intent(in)  :: x(0:m-1)
    real(real64), intent(out) :: y(0:n-1)
    integer, intent(in)         :: n, m
    integer :: i, j
    real(real64) :: acc
    do i = 0, n-1
      acc = 0.0_real64
      do j = 0, m-1
        acc = acc + A(i,j) * x(j)
      end do
      y(i) = acc
    end do
  end subroutine

  ! 6) 矩陣-向量乘法 (複數)
  subroutine matvec_complex(A, x, y, n, m)
    complex(real64), intent(in)  :: A(0:n-1, 0:m-1)
    complex(real64), intent(in)  :: x(0:m-1)
    complex(real64), intent(out) :: y(0:n-1)
    integer, intent(in)             :: n, m
    integer :: i, j
    complex(real64) :: acc
    do i = 0, n-1
      acc = (0.0_real64, 0.0_real64)
      do j = 0, m-1
        acc = acc + A(i,j) * x(j)
      end do
      y(i) = acc
    end do
  end subroutine

  ! 7) 矩陣-矩陣乘法 (實數) - 手寫雙迴圈
  subroutine matmul2d(A, B, C, n, p, m)
    real(real64), intent(in)  :: A(0:n-1, 0:p-1)
    real(real64), intent(in)  :: B(0:p-1, 0:m-1)
    real(real64), intent(out) :: C(0:n-1, 0:m-1)
    integer, intent(in) :: n, p, m
    integer :: i, j, k
    real(real64) :: val
    do i = 0, n-1
      do j = 0, m-1
        val = 0.0_real64
        do k = 0, p-1
          val = val + A(i,k) * B(k,j)
        end do
        C(i,j) = val
      end do
    end do
  end subroutine

  ! 8) 矩陣-矩陣乘法 (實數) - 直接呼叫內建 matmul
  subroutine matmul_builtin(A, B, C)
    real(real64), intent(in)  :: A(:,:), B(:,:)
    real(real64), intent(out) :: C(:,:)
    C = matmul(A, B)
  end subroutine

end module vecops
```

```fortran
program demo_zero_based
  use vecops
  implicit none
  integer, parameter :: n = 4, m = 3
  real(real64), dimension(0:n-1, 0:m-1) :: A
  real(real64), dimension(0:m-1)     :: x
  real(real64), dimension(0:n-1)     :: y
  real(real64), dimension(0:n-1, 0:m-1) :: C
  integer :: i, j

  ! 初始化 A 為遞增數列，A 的大小為 4x3，索引由 0 開始
  do i = 0, n-1
     do j = 0, m-1
       A(i,j) = real(i*10 + j)
     end do
  end do
  x = [(i, i = 0, m-1)]
  ! 矩陣-向量乘法
  call matvec(A, x, y, n, m)

  ! 矩陣-矩陣乘法 A(4x3) 與 B(3x2)
  real(real64), dimension(0:m-1, 0:1) :: B
  real(real64), dimension(0:n-1, 0:1) :: D
  do i = 0, m-1
     do j = 0, 1
        B(i,j) = real(i + j + 1)
     end do
  end do
  call matmul2d(A, B, D, n, m, 2)

  ! 顯示以驗證
  write(*,*) "A(0..3,0..2) ="
  do i = 0, n-1
     write(*,'(4(F6.1))') (A(i,j), j=0, m-1)
  end do
  write(*,*)
  write(*,*) "x = ", (x(i), i=0,m-1)
  write(*,*) "y = ", (y(i), i=0,n-1)
  write(*,*) "D(0..3,0..1) ="
  do i = 0, n-1
     write(*,'(2(F6.1))') (D(i,j), j=0,1)
  end do

end program
```

透過上述範例，你可以看到：泛型介面讓同一組操作可覆蓋不同數值型別；以 0 起點的實作協助你理解與驗證更貼近數值程式的實務情境。下一節將把這些運算元件與輸入檢核與容錯設計結合，建立更健全的介面與錯誤處理策略，為高階的資料處理與介面互動奠定穩固基礎。

### 節2：常見演算法概覽（加、乘、轉置）

本節聚焦三大核心運算：向量／矩陣的加法、乘法與轉置，並以 0 起點的實作來貼近數值計算的實務情境。為了清楚對照數學定義，先給出基本符號：對任意矩陣 $A \in \mathbb{R}^{m\times n}$、$B \in \mathbb{R}^{m\times n}$，其元素級加法為 $C_{ij} = A_{ij} + B_{ij}$；矩陣乘法定義為 $C = AB$，其中 $C_{ij} = \sum_{k=1}^{n} A_{ik} B_{kj}$；轉置則是 $A^{T}_{ij} = A_{ji}$。時間複雜度方面，向量/矩陣加法為 $O(mn)$；矩陣乘法在一般情況下為 $O(mnp)$；轉置為 $O(mn)$。在 Fortran 中，零起點陣列的運算可以直接用內建的陣列運算與轉置函式實作，並藉由泛型介面讓同一套介面對不同資料型別適用。

1) 加法：元素級相加與泛型介面

Fortran 允許直接對同形狀陣列做元素級加法：C = A + B。若要以泛型介面支援多種型別（整數、實數、複數等），可定義一個泛型介面集合，讓 add 在不同型別下呼叫對應實作，並讓函式具 elemental 屬性以支援陣列形態的運算。

- 公式：$C_{ij} = A_{ij} + B_{ij}$，若 A、B 相同形狀，C 即為結果。

- Fortran 程式示意（0 起點，實數矩陣，僅示範加法）：
```fortran
! 加法範例：0 起點矩陣 A 與 B 相同形狀，直接相加
program add_demo
  implicit none
  integer, parameter :: nr = 2, nc = 3
  real(8), dimension(0:nr-1,0:nc-1) :: A, B, C
  integer :: i, j

  ! 初始化：A 與 B
  do i = 0, nr-1
     do j = 0, nc-1
        A(i,j) = real(i*nc + j + 1, kind=8)
        B(i,j) = real((i*nc + j) + 10, kind=8)
     end do
  end do

  C = A + B

  print *, "A + B ="
  do i = 0, nr-1
     write(*,'(3(F6.1))') (C(i,j), j=0,nc-1)
  end do
end program
```

- 泛型介面增強（支援多型別的加法，保留元素級運算）：
```fortran
module gen_ops
  implicit none
  interface add
     module procedure add_real
     module procedure add_int
     module procedure add_complex
  end interface
contains
  elemental function add_real(a,b) result(res)
    real(8), intent(in) :: a,b
    real(8) :: res
    res = a + b
  end function
  elemental function add_int(a,b) result(res)
    integer, intent(in) :: a,b
    integer :: res
    res = a + b
  end function
  elemental function add_complex(a,b) result(res)
    complex(8), intent(in) :: a,b
    complex(8) :: res
    res = a + b
  end function
end module

```
用法：
```fortran
program use_gen_add
  use gen_ops
  implicit none
  real(8), dimension(0:1,0:2) :: Ar, Br, Cr
  integer, dimension(0:1,0:2) :: Ai, Bi, Ci
  Cr = add(Ar, Ar)  ! 自動呼叫 real 的 add_real
  Ci = add(Ai, Ai)  ! 自動呼叫 int 的 add_int
end program
```

2) 乘法：逐元素與矩陣乘法

- 逐元素乘法：若 A 與 B 形狀相同，D = A * B 即為逐元素乘法，對於科學計算常見的是點對點操作。

- 矩陣乘法：更常見的矩陣乘法用內建函式 matmul，定義為 $C = AB$，大小為 $C \in \mathbb{R}^{m \times p}$，其中 $A \in \mathbb{R}^{m \times n}$、$B \in \mathbb{R}^{n \times p}$，其元素為 $C_{ij} = \sum_{k=1}^{n} A_{ik} B_{kj}$。Fortran 的 MATMUL 提供高效率的實作，特別適合線性代數。

- Fortran 程式示意（0 起點，A 為 2×3，B 為 3×2）：
```fortran
program mul_demo
  implicit none
  integer, parameter :: m = 2, n = 3, p = 2
  real(8), dimension(0:m-1,0:n-1) :: A
  real(8), dimension(0:n-1,0:p-1) :: B
  real(8), dimension(0:m-1,0:p-1) :: C, D
  integer :: i,j

  ! 初始化 A(2x3) 與 B(3x2)
  do i = 0, m-1
     do j = 0, n-1
        A(i,j) = real(i*n + j + 1, kind=8)
     end do
  end do
  do i = 0, n-1
     do j = 0, p-1
        B(i,j) = real(i*p + j + 1, kind=8)
     end do
  end do

  ! 逐元素乘法（相同形狀）
  D = A * A  ! 這裡僅示範語法；實際可用 A*B，若形狀相同

  ! 矩陣乘法：C = A @ B
  C = MATMUL(A,B)

  print *, "A * B (element-wise) 內容："
  do i = 0, m-1
     write(*,'(3(F6.1))') (D(i,j), j=0,n-1)
  end do

  print *, "A * B (矩陣乘法) 內容："
  do i = 0, m-1
     write(*,'(2(F6.1))') (C(i,j), j=0,p-1)
  end do
end program
```
說明：若要展示逐元素乘法，確保 A 與 B 形狀一致；矩陣乘法使用 MATMUL，尺寸需符合 $A(m\times n)$ 與 $B(n\times p)$。

3) 轉置：把料件行列互換

- 公式：$(A^T)_{ij} = A_{ji}$，Fortran 提供內建轉置函式 transpose。

- Fortran 程式示意（0 起點）：
```fortran
program transpose_demo
  implicit none
  integer, parameter :: n = 2, m = 3
  real(8), dimension(0:n-1,0:m-1) :: A
  real(8), dimension(0:m-1,0:n-1) :: AT
  integer :: i,j

  do i = 0, n-1
     do j = 0, m-1
        A(i,j) = real(i*m + j + 1, kind=8)
     end do
  end do

  AT = transpose(A)

  print *, "A 與 A^T："
  do i = 0, m-1
     write(*,'(2(F6.1))') (AT(i,j), j=0,n-1)
  end do
end program
```
註：轉置通常影響記憶體存取模式，若要就地轉置需留意形狀是否相同與暫存空間需求；一般建議使用新的陣列儲存轉置結果。

4) 泛型介面與容錯設計的實務觀點

前述加法的泛型介面示範，實務上常伴隨輸入檢核與錯誤處理：在執行前先驗證 A 與 B 的形狀是否相容，若不相容，應輸出清楚錯誤訊息並中止運算，以避免神祕的運算結果或運行時例外。下列為一個簡化的檢核片段：

```fortran
logical function shapes_ok(A,B)
  implicit none
  real(8), intent(in) :: A(:,:), B(:,:)
  integer, allocatable :: sA(2), sB(2)
  sA = shape(A); sB = shape(B)
  shapes_ok = (rank(A) == rank(B)) .and. all(sA == sB)
end function
```

結合使用：
```fortran
program safe_add
  use gen_ops
  implicit none
  real(8), dimension(0:1,0:2) :: X, Y, Z
  if (shape(X) /= shape(Y)) then
     print *, "Error: input shapes 不相同"
     stop
  end if
  Z = add(X,Y)
end program
```

透過上述實作與檢核設計，零起點的向量與矩陣運算不僅在語法層面直覺，也能在型別與形狀變化時提供穩健的回饋，為後續的介面設計奠定基礎。

結語：於本節我們涵蓋了加法、乘法（逐元素與矩陣乘法）與轉置的基本實作與效能考量，並透過泛型介面與容錯設計，為更高階的資料處理與介面互動奠定穩固基礎。下一節將把這些運算元件與輸入檢核與容錯設計結合，建立更健全的介面與錯誤處理策略，進一步提升可用性與穩健性。

### 節3：簡單迭代法與收斂性

在目前的向量與矩陣運算基礎之上，簡單迭代法以固定點形式解決線性方程組 Ax = b，作為零基礎學習的核心案例之一。將矩陣 A 拆解為 A = D + L + U，其中 D 是對角矩陣，L 為嚴格下三角，U 為嚴格上三角。這樣的分解讓我們把原問題轉換為固定點問題：x = g(x)，其中
$$
A x = b \quad \Rightarrow \quad x^{k+1} = g(x^k) = D^{-1}\left(b - (L+U)x^k\right).
$$
此處的迭代矩陣為
$$
B = -D^{-1}(L+U),
$$
收斂的核心條件是譜半徑
$$
\rho(B) < 1.
$$
若 A 满足某些直對角支配條件，或者在對稱正定情況下有特定性質，Jacobi 迭代能保證收斂。換句話說，當迭代過程中誤差以收斂的速率衰減時，我們就獲得穩健的解。常見的可保證收斂的情況包括：A 為嚴格對角支配矩陣、A 為對稱正定且滿足某些條件等。為了直觀理解，可以把迭代過程想成把上一輪的解往 $g$ 的固定點靠攏，若洛必局部線性化後的線性部分的譜半徑小於 1，即可收斂。

為了清楚呈現，我們用簡短的例子說明。令
$$
A=\begin{bmatrix}4 & -1 & 0\\ -1 & 4 & -1\\ 0 & -1 & 3\end{bmatrix},\quad
b=\begin{bmatrix}15\\ 10\\ 10\end{bmatrix},
$$
A 的對角部分 D 為 diag(4,4,3)，L 與 U 分別為下、上三角部分。若初始向量 $x^0 = 0$，且以 Jacobi 迭代法求解，我們可以觀察到迭代式的局部誤差會在滿足條件時逐步減小，直到收斂到解向量 x*。

為了實作的可重用性，以下幾點值得注意：
- 固定點形式必須明確定義 g(x)，並用 D、L、U 重新組裝。
- 收斂性可透過分析 B 的譜半徑或採用直觀的支配條件來判斷，而非單靠機械迭代。
- 對於向量/矩陣運算的零基礎風格，應把索引從 1 改為 0（或其他自訂下界），以培養一致的介面設計。

以下提供一段展示 Jacobi 迭代的 Fortran 範例，使用零基礎索引以符合本章節的設計風格。程式核心在於將 A、b 拆解後執行 $x^{k+1} = D^{-1}(b - (L+U)x^k)$，並以最大變化量作為收斂檢查。

```fortran
program JacobiZeroBased
  implicit none
  integer, parameter :: n = 3
  integer :: i, j, iter, max_iter
  real(8) :: tol, diff, acc
  real(8), dimension(0:n-1,0:n-1) :: A
  real(8), dimension(0:n-1) :: b
  real(8), dimension(0:n-1) :: x, x_new

  ! A 和 b 的範例設置，矩陣 A 為 3x3 的嚴格對角支配矩陣
  A = reshape((/ 4.0d0, -1.0d0, 0.0d0, &
                -1.0d0, 4.0d0, -1.0d0, &
                 0.0d0, -1.0d0, 3.0d0 /), shape(A))
  b = (/ 15.0d0, 10.0d0, 10.0d0 /)

  x = 0.0d0
  x_new = 0.0d0
  tol = 1.0d-6
  max_iter = 100

  do iter = 1, max_iter
     ! Jacobi: x_new(i) = (b(i) - sum_{j != i} A(i,j) x(j)) / A(i,i)
     do i = 0, n-1
        acc = 0.0d0
        do j = 0, n-1
           if (j /= i) then
              acc = acc + A(i,j) * x(j)
           end if
        end do
        x_new(i) = (b(i) - acc) / A(i,i)
     end do

     diff = maxval(abs(x_new - x))
     x = x_new
     if (diff < tol) exit
  end do

  if (iter > max_iter) then
     print*, "Did not converge within max iterations"
  else
     print*, "Converged in ", iter, " iterations"
     print*, "x =", x
  end if
end program
```

實作要點補充：
- 絕對收斂性與收敛速度不僅取決於矩陣本身，還與初始猜測有關。合理的初值與適當的 tol 會影響收斂時間。
- 若 A 不是 Diagonally Dominant，Jacobi 可能不收斂，此時可以考慮 Gauss-Seidel、折半預測或其他穩健方法，或先進行前置條件化。
- 對於更高維度與浮點精度需求，建議用雙精度實作，並考慮向量化與並行化以提升效能。

結語：透過這個簡單迭代法的理論與實作範例，讀者能理解收斂性背後的數學意涵與實際的程式設計要點。下一節將把這些迭代元件與輸入檢核與容錯設計結合，打造更健全的介面與錯誤處理策略，進一步提升可用性與穩健性。

### 節4：效能考量與簡單優化技巧

在前述 Jacobi 迭代的理論與基礎實作之上，效能成為實作的關鍵點。對於小型科學計算中的向量與矩陣運算，核心成本通常落在矩陣-向量乘法與向量更新上；其計算量約為 $2n^2$ 次浮點運算，若能降低迭代次數或改善記憶體存取，整體時間將顯著進步。為此，本節提出幾個實作層面的簡單優化技巧，並示範在 Fortran 的典型寫法中如何落地。

要點整理
- 使用雙精度與穩定的向量化：若資料量較大、對精度要求較高，採用雙精度實作，並讓編譯器能更好地進行自動向量化與循環並行。
- 依賴高效的矩陣-向量運算：對於大多數情況，直接利用 Fortran 的內建運算子 matmul 或 BLAS 介面，能提供更穩定且高度優化的實作，減少自寫迴圈的風險與差異。公式  
  $$y = \mathrm{matmul}(A, x)$$ 
  的成本與穩定性通常優於逐元素的三層迴圈。
- 記憶體佈局與存取模式：Fortran 以左起第一個維度為最內部的變動，因此在撰寫自訂迴圈時，應避免造成跨欄的隨機存取；若要自行迴圈，宜以塊狀或改變運算順序提升快取命中率，或改以 matmul 取代手寫迴圈。
- 盡量避免每次迴圈動態配置：迭代過程中重複建立暫存陣列會污染快取，應在外層一次性分配並重用。
- 前置條件化與向量化提示：可先計算 Diagonal inverse、預先計算 diag(A) 等，藉由向量化與並行化結合的方式降低每次迭代成本。若要更進一步，考慮塊矩陣技術或平行化框架如 OpenMP。

以下為兩個實作範例，分別展示 Baseline 與 Optimized 的思路差異。

Baseline：簡單 Jacobi 逐元素迭代
```fortran
! Baseline Jacobi (簡單逐元素迭代)
subroutine jacobi_baseline(A, b, x, n, maxiter, tol)
  implicit none
  integer, intent(in) :: n, maxiter
  real(8), intent(in) :: A(n,n), b(n)
  real(8), intent(inout) :: x(n)
  real(8) :: xnew(n), sum
  integer :: iter, i, j

  do iter = 1, maxiter
     do i = 1, n
        sum = 0.0d0
        do j = 1, n
           if (i == j) cycle
           sum = sum + A(i,j) * x(j)
        end do
        xnew(i) = (b(i) - sum) / A(i,i)
     end do
     x = xnew
     if (maxval(abs(xnew - x)) < tol) exit
  end do
end subroutine
```

Optimized：利用矩陣-向量乘法與向量化前提的改良
- 核心變動：預先取 Diagonal diag(A)，利用 matmul 進行矩陣-向量乘法，並以向量運算完成更新，減少純迴圈的複雜度。
```fortran
! Optimized Jacobi：先計算 temp = A * x，再用對角線與 diagonal inverse 更新
subroutine jacobi_optimized(A, b, x, n, maxiter, tol)
  implicit none
  integer, intent(in) :: n, maxiter
  real(8), intent(in) :: A(n,n), b(n)
  real(8), intent(inout) :: x(n)
  real(8), allocatable :: diag(:), temp(:), xnew(:)
  integer :: iter

  allocate(diag(n), temp(n), xnew(n))
  do i = 1, n
     diag(i) = A(i,i)
  end do

  do iter = 1, maxiter
     temp = matmul(A, x)          ! 以 matmul 產生 A*x，成本較高但通常最佳化良好
     xnew = (b - (temp - diag*x)) / diag
     if (maxval(abs(xnew - x)) < tol) exit
     x = xnew
  end do

  deallocate(diag, temp, xnew)
end subroutine
```

補充說明
- 在大型或高需求情境中，若可接受外部依賴，直接轉用 BLAS/LAPACK 的語法與介面，常能取得更穩定且可攤平成本的效能，尤其在多核心與向量單元上表現突出。
- 為了更進一步的並行化，可以在 Fortran 檔案中加入 OpenMP 指令，如在迭代外層加上 parallel do，或使用 do concurrent 以協助編譯器自動向量化；當資料量增長時，這類併行化的收益會更為明顯。

透過這些簡單的實作與測試，讀者能感受到效能與穩健性之間的折衷，並在後續章節中學習如何把這些迭代元件與輸入檢核、容錯設計結合，打造更健全的介面與錯誤處理策略，提升整體可用性與穩健性。下一節將繼續探討輸入檢核與容錯設計的實作要點，並與前述迭代元件整合成更完整的科學計算介面。

### 節5：實作範例：矩陣乘法與向量運算

在本節中，透過小型的矩陣乘法與向量運算實作，示範如何讓程式具備可向量化的特性，同時保留清晰的介面與測試管道。為了協助編譯器自動向量化，我們會結合 do concurrent 的寫法與純數值運算的區塊，以便比較不同實作的效能與穩健性。矩陣乘法的基本定義為

$$ C_{i,j} = \sum_{k=1}^{K} A_{i,k} B_{k,j} $$

這是大多數科學計算中最核心的運算之一。我們也提供向量運算的基本元件：AXPY、點積 dot，以及向量長度等工具，作為後續介面設計與測試的組件。

以下提供可直接編譯的 Fortran 程式片段，分成模組與驅動測試兩部分。模組內含三個核心功能：AXPY(y ← y + α x)、dot 乘積，以及以 do concurrent 實作的矩陣乘法子程序 matmul_concurrent，該版本讓外層索引較易被向量單元與快取預取所利用。

```fortran
! 節5-1: 介面模組
module la_examples
  implicit none
  integer, parameter :: dp = selected_real_kind(12, 307)

contains

  ! y := y + a * x
  subroutine axpy(n, a, x, y)
    integer, intent(in) :: n
    real(dp), intent(in) :: a
    real(dp), intent(in) :: x(n)
    real(dp), intent(inout) :: y(n)
    y = y + a * x
  end subroutine axpy

  ! dot(x, y) = sum_i x_i * y_i
  function dot(n, x, y) result(res)
    integer, intent(in) :: n
    real(dp), intent(in) :: x(n), y(n)
    real(dp) :: res
    res = sum(x * y)
  end function dot

  ! C := A * B, 使用 do concurrent 以促進向量化
  subroutine matmul_concurrent(M, N, K, A, B, C)
    integer, intent(in) :: M, N, K
    real(dp), intent(in) :: A(M,K)
    real(dp), intent(in) :: B(K,N)
    real(dp), intent(out) :: C(M,N)
    integer :: i, j, k

    do concurrent (i = 1:M, j = 1:N)
      C(i,j) = 0.0_dp
      do k = 1, K
        C(i,j) = C(i,j) + A(i,k) * B(k,j)
      end do
    end do concurrent
  end subroutine matmul_concurrent

end module la_examples
```

```fortran
! 節5-2: 驅動測試程序
program test_la
  use la_examples
  implicit none

  integer, parameter :: M = 4, K = 3, N = 5
  real(dp), dimension(M,K) :: A
  real(dp), dimension(K,N) :: B
  real(dp), dimension(M,N) :: C1, C2
  real(dp) :: diff

  integer :: i, j

  call random_seed()
  call random_number(A)
  call random_number(B)

  ! 使用自訂的矩陣乘法實作
  call matmul_concurrent(M, N, K, A, B, C1)

  ! 與內建 matmul 比對以驗證正確性
  C2 = matmul(A, B)

  diff = maxval(abs(C1 - C2))
  print *, "最大誤差 (自訂 vs matmul): ", diff

  ! 演示向量運算元件
  real(dp), dimension(N) :: x, y
  real(dp) :: t

  call random_number(x)
  call random_number(y)
  t = dot(N, x, y)
  print *, "向量點積 dot(x,y) = ", t

  call axpy(N, 2.0_dp, x, y)
  print *, "執行 y = y + 2*x 後的 y = ", y

end program test_la
```

說明與討論要點：

- 結構與介面設計：模組集中管理三個核心元件，方便在不同的科學計算介面中重用。AXPY 提供最常見的烽火式向量更新，dot 用於評估向量相似度與投影等，矩陣乘法則以 do concurrent 的外層並行模式實作，便於編譯器在迭代層級之外自動向量化。
- 向量化與快取考量：Fortran 的列主序記憶體布局意味著 A(i,k) 在 kis 維度上存取可能較不連續。實務上可考慮把內層迴圈改為 k 為外層、i 與 j 為內層的寫法，或利用區塊化與 BLAS 介面提升效能。不過本章的重點在於讓讀者看到「可向量化」的結構與測試流程，而非一次性最佳化。
- 正確性驗證：與內建 matmul 的比較提供穩健性檢查；dot 與 axpy 的簡易測試則讓讀者理解各元件在更大系統中的組裝方式。顯示公式支持的特徵包括：矩陣乘法的點積結構、向量化路徑與縮放更新，以及以簡單測試作為回歸驗證的基本策略。

公式回顧與要點：矩陣乘法的基本框架與多轉置路徑可透過以下表述理解

- 矩陣乘法：$C = A B$，$C_{i,j} = \sum_{k=1}^K A_{i,k} B_{k,j}$，此處外層使用 Do Concurrent 的寫法有助於自動向量化與平行化。
- 向量運算：$y \leftarrow y + \alpha x$，點積：$\mathrm{dot}(x,y) = \sum_i x_i y_i$，長度歸結：$\|x\|_2 = \sqrt{\sum_i x_i^2}$，這些基本運算可作為更完整科學介面的骨架。

透過這些實作與測試，讀者能感受到向量化與穩健性之間的折衷，也為後續章節在輸入檢核與容錯設計中，將這些元件組合成更穩健的科學介面奠定基礎。

下一節將繼續探討輸入檢核與容錯設計的實作要點，並與前述迭代元件整合成更完整的科學計算介面。

## 第 2 章：章八：檔案 I/O 與資料管理

### 節1：文字檔案的讀寫

在科學計算中，文字檔案是最常見的輸入輸出介面。本文以「零基礎」的 FORTRAN 學習者為對象，介紹文字檔案的開啟、讀取與寫入，以及如何在讀寫過程中執行基本的資料檢核與容錯設計。先把文字資料視為向量的集合，例如每筆記錄為 r = (r1, r2, ..., rm)，則長度可表示為 $|r|_2 = \sqrt{\sum_j r_j^2}$；若要計算兩向量間的關聯，內積為 $\langle x, y \rangle = \sum_i x_i y_i$ 且與歸一化長度相關的分析常用公式也可直接套用。這些公式在與檔案資料結合時，能幫助我們把輸入資料轉換為可操作的數值介面：例如平均值、標準差、以及向量化處理的骨架。

1) 基本概念與設計原則
- 文字檔案通常使用「格式化輸出」(formatted) 或「非格式化輸出」(unformatted) 的模式。若要跨平台與易於除錯，建議以格式化輸出/輸入為主，並在程式中嚴格處理檔案開啟與結束狀態。  
- 開啟檔案時，避免直接使用固定的單位號碼，改以 newunit/position 參數管理，並搭配 iostat 檢查錯誤。常見的流程是：open -> loop 讀/寫 -> close。  
- 讀取時的容錯設計要能辨識「結束（EOF）」與「錯誤」兩種情形，並對輸入資料進行基本清洗，如略過空行或註解行。

2) 程式設計要點（語法重點與風格）
- 以內部檔案（internal file）讀取方式，先把整行文字讀入字串，再用 read(line, *) 解析欄位，能提高對原始檔案格式變化的容錯性。  
- 使用 iostat 監控每次 read 的結果，遇到 EOF 或格式錯誤時，給出清楚的錯誤訊息並結束迴圈，避免資料污染或崩潰。  
- 當需要輸出到檔案時，使用可讀性高的格式化格式，例如欄位對齊與固定小數位，便於日後比對與重跑。

3) 範例：讀取一行一數字的檔案並計算平均值
以下為簡單實作，說明如何以內部檔案讀取與基本容錯實作，並輸出均值。你可自行修改檔名與欄位數，以符合實際資料格式。

```fortran
! 範例1：從 numbers.txt 逐行讀取一個實數，計算平均值
program read_numbers
  implicit none
  integer :: unit_in, ios
  integer, parameter :: maxn = 10000
  real, dimension(maxn) :: a
  integer :: n
  real :: x, sum
  character(len=256) :: line

  sum = 0.0
  n = 0

  open(newunit=unit_in, file='numbers.txt', status='old', &
       action='read', form='formatted', iostat=ios)
  if (ios /= 0) then
     write(*,*) 'Cannot open input file.'
     stop
  end if

  do
     read(unit_in, '(A)', iostat=ios) line
     if (ios /= 0) exit            ! EOF 或讀取錯誤皆退出
     read(line, *) x
     n = n + 1
     a(n) = x
     sum = sum + x
  end do

  close(unit_in)

  if (n > 0) then
     write(*, '(A, F6.3)') 'mean =', sum / real(n)
  else
     write(*, '(A)') 'no data'
  end if
end program
```

4) 範例：寫入與附加文字內容
為了展示寫入檔案的基本作法，以下程式輸出一個簡單的標題列與數值表格。你可以調整欄位寬度與格式化字串以符合需求。

```fortran
! 範例2：寫入數值表格到 output.txt
program write_table
  implicit none
  integer :: unit_out, i
  integer, parameter :: n = 5
  real :: v(n)

  do i = 1, n
     v(i) = real(i) * 0.5
  end do

  open(newunit=unit_out, file='output.txt', status='replace', &
       action='write', form='formatted')

  write(unit_out, '(A)') 'index  value'
  do i = 1, n
     write(unit_out, '(I6, 1X, F6.2)') i, v(i)
  end do

  close(unit_out)
end program
```

5) 附加與容錯的最佳實踐
- 使用 iostat 判斷讀取狀態，遇到 EOF 值為 0 附近時表示結束；非 0 則為錯誤，應提供清晰的錯誤訊息並適時清理資源。  
- 將檔案開啟狀態與路徑獨立於核心邏輯，方便追蹤與單元測試。  
- 若資料包含註解或空行，可在讀取後的解析階段先過濾：例如在 line 的第一個字元是 '#' 時跳過。  
- 對於跨平台開發，盡量使用可攜性的格式化輸出，如固定寬度欄位與固定小數位，並在正式分析前先做簡單的輸入資料驗證（例如欄位數、型別、範圍等）。相關數學符號可用下列公式描述資料的統計性質，例如平均值 μ 與標準差 σ：
  -  Inline: $\mu = \frac{1}{n} \sum_{i=1}^n x_i$
  -  Inline: $\sigma = \sqrt{\frac{1}{n-1}\sum_{i=1}^n (x_i - \mu)^2}$
  -  Display: $\langle x, y \rangle = \sum_i x_i y_i, \quad \|x\|_2 = \sqrt{\sum_i x_i^2}.$
  這些公式在讀入資料後的後續分析階段，可以直接用於驗證與摘要。

6) 小結與設計要點
- 文字檔案的 I/O 是穩健科學介面的基礎元件，需與錯誤處理、資料清洗、以及輸出格式設計共同考量。  
- 採用內部檔案解析與 iostat 驗證能提升穩健性，同時讓未來的輸入檢核與容錯設計更易組裝成完整的科學介面。  
- 從現在起，讀寫元件將與後續章節的「輸入檢核與容錯設計」整合，形成可重複使用的輸入模組。

下一節將進一步探討輸入檢核與容錯設計的實作要點，並與前述迭代元件整合成更完整的科學計算介面。

### 節2：格式化輸出與讀取

格式化的輸出與讀取是穩健科學介面的第一層屏障。良好的格式化不僅讓人類可讀，亦便於機器解析與自動化檢核。此節將結合內部檔案解析與 iostat 驗證，建立可重複使用的輸出模組，並為後續的輸入檢核與容錯設計奠定基礎。常用的格式控制字元有 $F9.4$, $E12.5$, $A$, $I6$ 等，請在設計時統一命名風格，避免混用。

- 格式設計原則
  - 先定義欄寬與對齊，例如固定寬度欄位與單位，避免輸出時的位元偏移。使用格式字串或 FORMAT 物件產生一致的輸出。
  - 對浮點數與整數採用分明的欄寬與小數位，例如 $F8.3$、$I6$，並用適當的填充字元，確保對齊。
  - 對欄位含義使用註解或標籤，在檔案模式與文字檔中保持自描述性。

- 讀取時的穩健策略
  - 儘量使用顯式格式（格式字串），以避免 list-directed 解析的不可預期。
  - 結合 iostat 取得 I/O 狀態，若 iostat 回傳非零值，立即進入錯誤處理流程。
  - 對關鍵欄位設置最小可接受範圍，並在解析錯誤時提供回歸檢核。

- 內部檔案解析與對外介面
  - 以內部檔案（internal file）產出中間字串，再由解析模組取出欄位值。此方式有助於測試與模組化，並降低直接磁碟 I/O 的耦合。
  - 對外介面以結構化記憶體資料傳遞，輸出格式與輸入格式分離，便於未來的檢核與容錯時模組重用。

- 範例一：輸出到內部字串與從字串讀回
```fortran
! 範例：內部檔案格式化輸出與讀取
program fmt_io_internal
  implicit none
  real :: x, y
  character(len=200) :: buf
  integer :: ios

  x = 12.3456
  y = 78.9012

  ! 將兩個數值格式化輸出到內部字串
  write(buf, '(F8.4,1X,F8.4)') x, y

  ! 從內部字串解析回來
  read(buf, '(F8.4,1X,F8.4)') x, y

  print *, 'x=', x, ' y=', y
end program fmt_io_internal
```

- 範例二：與實體檔案的格式化 I/O 與 iostat
```fortran
program fmt_io_disk
  implicit none
  real :: a, b
  integer :: iunit, ios

  a = 3.14159
  b = 2.71828
  open(unit=10, file='data.txt', status='replace', action='write', iostat=ios)
  if (ios /= 0) then
     print *, 'open failed', ios
     stop
  end if
  write(10, '(F8.4,1X,F8.4)') a, b
  close(10)

  open(unit=10, file='data.txt', status='old', action='read', iostat=ios)
  if (ios /= 0) then
     print *, 'open read failed', ios
     stop
  end if
  read(10, '(F8.4,1X,F8.4)', iostat=ios) a, b
  if (ios /= 0) then
     print *, 'read failed', ios
     stop
  end if
  close(10)

  print *, 'Read values:', a, b
end program fmt_io_disk
```

- 內部檔案解析的好處
  - 減少外部因素對輸入檢核的干擾，讓檔案格式與解析邏輯可獨立測試。
  - 透過 iostat 回報的狀態碼與輸出字串的長度檢查，能快速偵測截斷或格式錯誤。

- 進階提示
  - 將重複的輸出格式抽成模組，提供可重複呼叫的介面，如 write_fmt(x,y) 與 read_fmt(buf,vars)。
  - 為重要欄位建立單位測試與容錯路徑，確保在各種邊界情況下仍穩健輸入輸出。

結尾過渡：透過上述格式化輸出與讀取的實作原則，下一節將聚焦於輸入檢核與容錯設計的實作要點，並展示如何將這些元件整合為更完整的科學計算介面。

### 節3：二進位檔案與資料儲存

在本節，我們聚焦二進位檔案的穩健寫入與載入，利用 Fortran 的 stream 介面將資料以原生二進位儲存。重點是以固定欄位佈局寫入，並在每筆寫入後檢查 I/O 狀態，能快速偵測截斷或格式錯誤。為了降低重複，建立重複使用的輸出/輸入介面，如 write_fmt(x,y) 與 read_fmt(buf,vars)，並搭配單位測試與容錯路徑。

核心原理
- 使用 stream I/O，資料以二進位形式寫入，避免格式化字串的截斷風險。若檔案結束或異常，iostat 會回傳非零；此時需採取適當的容錯策略，避免後續解析失效。
- 以固定順序寫入欄位，如整數後接實數，共佔用固定位元數，方便跨平台比對與邊界檢查。若需要跨平台交換，建議明確選用 int32/real64 的位元大小，並以 transfer/位元長度概念做嚴格控制。

範例模組：binary_store
以下為可重用的二進位寫入/讀取介面，提供 write_fmt 與 read_fmt 的基本實作。為符合需求，write_fmt(x,y) 與 read_fmt(buf,vars) 的語意以不同型別組合與順序實作為多載介面，並附上 I/O 錯誤回報機制。

```fortran
module binary_store
  use iso_fortran_env, only: int32, real64
  implicit none
contains

  ! 介面：寫入一組 (整數, 實數)
  interface write_fmt
     module procedure write_fmt_int64_real64
     module procedure write_fmt_real64_int64
  end interface

  ! 介面：從裝有資料的 buffer 取出一組 (整數, 實數)
  interface read_fmt
     module procedure read_fmt_int64_real64
     module procedure read_fmt_real64_int64
  end interface

  ! 寫入: i, f
  subroutine write_fmt_int64_real64(unit, i, f, ios)
     integer, intent(in)  :: unit
     integer(kind=int32), intent(in) :: i
     real(kind=real64), intent(in)  :: f
     integer, intent(out), optional :: ios
     integer :: io

     write(unit) i
     write(unit) f
     if (present(ios)) ios = 0
  end subroutine

  ! 寫入: f, i
  subroutine write_fmt_real64_int64(unit, f, i, ios)
     integer, intent(in)  :: unit
     real(kind=real64), intent(in) :: f
     integer(kind=int32), intent(in) :: i
     integer, intent(out), optional :: ios
     write(unit) f
     write(unit) i
     if (present(ios)) ios = 0
  end subroutine

  ! 讀取: i, f
  subroutine read_fmt_int64_real64(unit, i, f, ios)
     integer, intent(in)  :: unit
     integer(kind=int32), intent(out) :: i
     real(kind=real64), intent(out) :: f
     integer, intent(out), optional :: ios
     integer :: io

     read(unit) i
     io = 0
     if (io == 0) then
        read(unit) f
     endif
     if (present(ios)) ios = io
  end subroutine

  ! 讀取: f, i
  subroutine read_fmt_real64_int64(unit, f, i, ios)
     integer, intent(in)  :: unit
     real(kind=real64), intent(out) :: f
     integer(kind=int32), intent(out) :: i
     integer, intent(out), optional :: ios
     integer :: io

     read(unit) f
     if (io == 0) then
        read(unit) i
     endif
     if (present(ios)) ios = io
  end subroutine

end module
```

實作要點與說明
- 資料大小與順序需固定，方便日後檢查。上例以 int32 與 real64 為標準，可以確保在大多數平台上的對應位元長度。
- 錯誤處理透過 iostat 變數 (此處命名為 ios) 或回傳值判斷，遇到非零代表截斷、格式錯誤或檔案問題。實務中可以加入跳過不可讀的區段、記錄錯誤、或中止程序的策略。
- 為了單元測試友善，請於寫入後關閉檔案、再以唯讀模式開啟並執行對應的讀取測試，驗證端到端的 round-trip。

單元測試與容錯路徑
1) 基本回帶測試
- 將 i=123、f=4.5 帶入寫入，接著從同一檔案讀回，驗證 i 與 f 與原值相符，且 iostat 為 0。
2) 檔尾截斷測試
- 只寫入部分資料，嘗試完整讀取，預期 iostat 非零或讀取失敗，並在程式中提供友善的錯誤訊息與清理機制。
3) 格式錯誤情境
- 人為修改檔案內容，模擬非預期的位元序列，檢查 read_fmt 的容錯回報，是否能避免崩潰並提供清晰訊息。

使用範例
```fortran
program demo_binio
  use binary_store
  implicit none
  integer(kind=int32) :: i
  real(kind=real64)  :: f
  integer :: unit, ios
  character(len=20) :: fname = 'sample.bin'

  unit = 10
  open(unit=unit, file=fname, access='stream', form='unformatted', status='replace')
  call write_fmt_int64_real64(unit, 123, 4.56d0, ios)
  call write_fmt_real64_int64(unit, 7.89d0, 42, ios)
  close(unit)

  open(unit=unit, file=fname, access='stream', form='unformatted', status='old')
  call read_fmt_int64_real64(unit, i, f, ios)
  print *, 'read1:', i, f
  call read_fmt_real64_int64(unit, f, i, ios)
  print *, 'read2:', f, i
  close(unit)
end program
```

結語與過渡
透過上述將格式化輸出與讀取的實作原則，建立了穩健的二進位儲存與解析介面，並配合單元測試與容錯路徑強化魯棒性。下一節將聚焦於輸入檢核與容錯設計的實作要點，並展示如何將這些元件整合為更完整的科學計算介面。

### 節4：錯誤處理與檔案結束

在前節的實作中，我們已建立格式化與二進位資料的存取介面。實際執行時，檔案已可能遇到多種狀況：檔案不存在、磁碟 I/O 錯誤、或是已經讀到結尾。為了讓程式在長時間執行或批次作業中穩健運作，必須將錯誤處理與檔案結束的路徑清楚定義。Fortran 提供 IOSTAT、END、ERR 等機制，讓我們能在發生異常時以可控的方式轉移執行流程；同時使用 END=label 來捕捉檔案結尾，避免錯誤解讀 EOF 為一般錯誤。

核心要點
- IOSTAT（整數型變數）用於捕捉讀寫的狀態。$IOSTAT = 0$ 表示正常；非零表示發生 I/O 錯誤。其具體值在不同編譯器可能略有差異，因此不可依賴數值本身判斷 EOF。
- END=label 指定當遇到終止條件（End-of-File）時，控制轉移到指定的標籤。這是處理 EOF 的常用方式，能避免把 EOF 誤認為一般錯誤。
- ERR=label 與 IOSTAT 結合時，遇到嚴重錯誤就跳到 ERR 標註處理，讓程式具同一入口的錯誤路徑。
- EOR 與其他雜項狀態在一般文字格式輸入較常見，若以單純的二進位串流讀取，END 與 ERR 的組合更為穩健。

示例：使用 END 與 IOSTAT 的基本讀取迴圈
```fortran
program eof_error_demo
  implicit none
  integer :: unit = 10
  integer :: ios
  real(8) :: v
  open(unit=unit, file='samples.bin', access='stream', form='unformatted', status='old')
  do
     ! 遇 EOF 將跳轉至 EOF 標籤；遇其他 I/O 錯誤跳轉至 ERR 標籤
     read(unit, iostat=ios, end=EOF, err=ERR) v
     if (ios /= 0) then
        ! 若未使用 END/ERR，這裡的判斷也可用
        print *, 'I/O error:', ios
        exit
     endif
     print *, 'read:', v
  end do
EOF:
  print *, 'End of file reached'
  close(unit)
  stop
ERR:
  print *, 'Fatal I/O error occurred'
  close(unit)
  stop
end program
```

解說
- 此範例以二進位串流方式從檔案讀取實數資料，若遇 EOF 會直接跳到 EOF 標籤，並輸出結束訊息；若遇到其他 I/O 錯誤，則跳到 ERR 標籤並結束程式。
- IOSTAT 在正常路徑會保持為 0；若非 0，代表某些非 EOF 的錯誤，此時可以印出訊息並結束。注意：當 END=、ERR= 被指定時，控制轉移的機制會優先於一般的 IOSTAT 檢查，因此在循環內仍應保留對 ios 的判斷以支援非 EOF 的錯誤情形。
- 局部狀態的清晰管理有助於單元測試：可用虛擬資料檔模擬 EOF、磁碟錯誤或檔案缺失，逐步驗證程式的容錯路徑。

延伸設計與最佳實踐
- 為避免重複撰寫相同的錯誤處理邏輯，可把 I/O 讀取包裝成子程式或模組，在不同的資料型別與格式上共用錯誤分支，並在主流程中只專注於資料處理。
- 對於需要長時間批次運算的程式，建議在每個重大 I/O 之後檢查並記錄錯誤狀態，避免在後續運算中才發現檔案已中斷，造成難以追蹤的錯誤。
- 結合單元測試：建立測試檔案，模擬 EOF、突然中斷、以及格式錯誤，確認在不同 IOSTAT 與 END/ERR 組合下的行為是否符合預期。

結語與過渡
透過上述將錯誤處理與 EOF 的實作原則，建立穩健的檔案 I/O 流程與容錯路徑。下一節將聚焦於輸入檢核與容錯設計的實作要點，並展示如何將這些元件整合為更完整的科學計算介面。

### 節5：多檔案資料的整理與範例

在前一節的基礎上，這一小節聚焦「多檔案資料的整理與範例」。核心目標是把分散於多個檔案的觀測資料整合成一個整體資料集，同時維持穩健的 I/O 流程與錯誤容錯路徑。實務上常見情境是分批產出資料時間序列，或是將不同來源的同型資料併入分析流程。設計原則如下：
- 以檔案清單為入口，逐檔讀取，遇到結束（End-of-File, EOF）或錯誤（Format error、I/O 異常）時，分別記錄狀態，避免在後續運算才發現問題。
- 統一資料型態與欄位順序，確保跨檔整併時可預期地拼接為長序列；必要時以元資料記錄檔案來源與時間範圍。
- 使用 IOSTAT 與 END/ERR 的組合，區分「正常到達檔尾」與「發生檔案格式錯誤或中斷」的情境，讓後續流程能以穩健的容錯路徑繼續或中止。

範例資料格式與整理策略
- 每一筆紀錄包含兩欄：時間 t 與值 v，兩者皆為浮點數，欄位以空白分隔。
- 檔案集合 A = {a1, a2, a3, …}，最終結果為把每個檔案的紀錄串接成一個長序列。
- 合併後的統計量可先計算總筆數 N、平均值 $\bar{v}$ 以及時間跨度，公式分別為
  - 總筆數：$N = \sum_i n_i$
  - 平均值：$\bar{v} = \frac{1}{N}\sum_{i=1}^N v_i$
  - 時間範圍：$[t_{\min}, t_{\max}]$

以下為實作要點與範例程式碼。為方便閱讀，核心資料型態與 I/O 邏輯放在模組中，主程式僅負責整併與簡易分析。

範例程式碼區塊：模組與讀檔子程序
```fortran
! data_io.f90
module data_io
  implicit none
  type :: DataRecord
     real :: t
     real :: v
  end type DataRecord
contains
  subroutine read_file(fname, recs, n, eof, ok)
    character(len=*), intent(in) :: fname
    type(DataRecord), allocatable, intent(out) :: recs(:)
    integer, intent(out) :: n
    logical, intent(out) :: eof, ok

    integer :: iunit, ios
    real :: tval, vval
    n = 0
    eof = .false.
    ok = .true.
    allocate(recs(0))

    open(newunit=iunit, file=fname, status='old', &
         action='read', form='formatted', iostat=ios)
    if (ios /= 0) then
       ok = .false.
       eof = .true.        ! 以 EOF 視為尚未成功取得資料
       return
    end if

    do
       read(iunit, *, iostat=ios) tval, vval
       if (ios == -1) then
          eof = .true.      ! END OF FILE
          exit
       else if (ios /= 0) then
          ok = .false.      ! 其他 I/O 錯誤
          exit
       else
          n = n + 1
          if (size(recs) < n) then
             call ensure_size(recs, n)
          end if
          recs(n) = DataRecord(tval, vval)
       end if
    end do

    close(iunit)
  end subroutine read_file

  subroutine ensure_size(arr, newN)
    type(DataRecord), allocatable, intent(inout) :: arr(:)
    integer, intent(in) :: newN
    type(DataRecord), allocatable :: tmp(:)
    if (size(arr) < newN) then
       allocate(tmp(newN))
       if (size(arr) > 0) tmp(1:size(arr)) = arr
       deallocate(arr)
       arr => tmp
    end if
  end subroutine ensure_size
end module data_io
```

範例程式碼區塊：主程式與整併邏輯
```fortran
! main.f90
program multi_file_aggregate
  use data_io
  implicit none

  character(len=*), dimension(:), allocatable :: files
  type(DataRecord), allocatable :: all(:)
  integer :: totalN, n
  logical :: eof, ok
  integer :: i, nf

  ! 假設三個資料檔案
  allocate(files(3))
  files = ['data1.txt','data2.txt','data3.txt']

  totalN = 0
  allocate(all(0))

  do i = 1, size(files)
     type(DataRecord), allocatable :: part(:)
     integer :: m
     call read_file(trim(files(i)), part, m, eof, ok)
     if (.not. ok) then
        write(*,'(A, I0)') 'Error reading ', i, 'th file: ', trim(files(i))
        cycle
     end if
     if (m > 0) then
        call append_records(all, totalN, part, m)
     end if
     if (eof) then
        ! 已到檔尾，繼續下一個檔案以確認結束條件
        nullify(part)
     endif
  end do

  ! 簡單分析：輸出長度與平均值
  write(*,'(A,I6)') 'Total records:', totalN
  if (totalN > 0) then
     real :: vsum
     vsum = 0.0
     do i = 1, totalN
        vsum = vsum + all(i)%v
     end do
     write(*,'(A,F8.3)') 'Mean v:', vsum / real(totalN)
  end if
contains
  subroutine append_records(dst, ndst, src, nsrc)
     type(DataRecord), allocatable, intent(inout) :: dst(:)
     integer, intent(inout) :: ndst
     type(DataRecord), allocatable, intent(in) :: src(:)
     integer, intent(in) :: nsrc
     type(DataRecord), allocatable :: tmp(:)

     if (nsrc <= 0) return
     allocate(tmp(ndst + nsrc))
     if (ndst > 0) tmp(1:ndst) = dst
     tmp(ndst+1:ndst+nsrc) = src(1:nsrc)
     deallocate(dst)
     dst => tmp
     ndst = ndst + nsrc
  end subroutine append_records
end program multi_file_aggregate
```

單元測試與測試檔案設計
- 為了驗證不同情境，建立測試檔案：
  - data_good.txt：內容完整、格式正確，例如
    0.0 1.0
    1.0 2.0
  - data_eof_mid.txt：在中段就結束，模擬突然中斷的情境。
  - data_badfmt.txt：某行包含非數字字元，如
    0.0 x
  - data_empty.txt：空檔案，測試 EOF 處理。
- 測試預期：
  - 對 data_good.txt，ok 應為 .true.，eof 可能為 .true. 或 .false. 取決於是否到檔尾。
  - 對 data_badfmt.txt，ok 應為 .false.，ERR/END 的組合應觸發錯誤路徑。
  - 對 data_eof_mid.txt，若以 END=label 控制，應在結尾處觸發 eof，並記錄已讀取的筆數。
- 單元測試可用以下步驟進行：
  - 自動產生測試檔案，放置於 test/ 目錄。
  - 逐檔呼叫 read_file，檢查回傳的 n、eof、ok 與 part 的大小。
  - 對不同檔案組合，檢查合併後的 N、平均值是否符合預期。

結語與過渡
透過上述多檔案資料整理與範例，已建立穩健的檔案 I/O 流程與容錯路徑，並展示如何在實務中進行單元測試以模擬 EOF、突然中斷、以及格式錯誤的情境。下一節將聚焦於輸入檢核與容錯設計的實作要點，並展示如何將這些元件整合為更完整的科學計算介面，進一步提升整體的魯棒性與可維護性。

## 第 3 章：章九：實務專案與除錯技巧

### 節1：專案規劃與模組化設計

本節聚焦於把「FORTRAN 零基礎」專案拆成清楚的模組、定義穩健的介面與契約，讓多檔案 I/O 與合併邏輯能在變動中保持穩定。前文已提到傳的 n、eof、ok 與 part 的大小，以及不同檔案組合下合併後的 N 與平均值應該符合預期。本節先定義專案結構、介面契約，並以範例草案說明模組間的分工與資料流向。

需求與契約
- 目標: 從多個檔案讀取區塊資料，合併後計算總區塊數 N_total 與全域平均值 mean，且在每個區塊讀取時回報 ok、eof。
- 資料契約：每個區塊用一個 Block 類型表示，其中 n 為區塊大小，data(:) 為實數資料序列。合併公式
  - $$N_{\text{total}} = \sum_{i=1}^{k} n_i$$
  - $$\text{mean} = \frac{\sum_{i=1}^{k} \sum_{j=1}^{n_i} x_{i,j}}{N_{\text{total}}}$$
  其中 k 為檔案數。透過這樣的設計，可以在不同檔案組合下驗證合併結果與平均值是否符合預期。

模組化設計原則
- 單一職責原則：每個模組負責單一職能，例如檔案 I/O、區塊資料結構、合併與統計、測試輔助。這樣便於單元測試與模組替換。
- 明確介面：對外暴露的型別與子程式以最小集為主，避免暴露內部實作細節。
- 可替代性：以介面契約實作，允許用不同的 I/O 後端（磁碟、快取、模擬資料）而不改動核心邏輯。
- 錯誤與容錯：統一的狀態碼與例外路徑，當遇到 EOF、I/O 錯誤或格式錯誤時，能清楚回報並讓上層決定處理策略。

模組清單與責任分工
- data_types 模組：定義 Block 類型與共用資料結構。
- fileio 模組：負責檔案開啟、讀區塊與關閉的介面；輸出 ok 與 eof 的旗標。
- merger 模組：負責多區塊之合併、統計與平均值計算。
- test_harness 模組：提供模擬資料與單元測試用的區塊序列，支援 EOF、中斷與格式錯誤場景。
- main 或 runner：協調多模組工作流程，實作整體執行邏輯與結果輸出。

介面設計要點
- Block 類型應公開，以便不同模組都能建立與傳遞資料。
- read_block 單元應回傳 ok、eof，且當 ok 不成立時，決定是否要繼續或終止。
- 合併介面應接受多個 Block 陣列，回傳 N_total 與 mean，並提供逐步增量更新的能力以便測試。

實作風格與容錯策略
- 錯誤碼設計：0 表示成功，-1 表示 EOF，-2 表示 I/O 錯誤，-3 表示格式錯誤。
- 對不同檔案組合，保留整體 N 與 mean 的正確性檢查機制，若任一檔案發生嚴重錯誤，提供可選擇的回復路徑（如跳過該檔案、或停止整體流程）。
- 測試導向開發：先定義預期的區塊序列與結果，再實作對應的單元測試案例。

程式設計與流程
- 使用模組化的工作流程，搭配版本控制與簡易 CI，確保每次變更都經過測試。
- 對 I/O 的單元測試，使用「模擬資料源」來替代實際檔案，以便控制 EOF 與格式錯誤情境，符合「可重現的單元測試」原則。
- 介面契約與簽名要清楚註解，便於他人快速理解與重用。

範例草案：Fortran 結構骨架
下面提供一個簡化的 Fortran 模組骨架，展示 data_types、fileio 之間的基本互動結構與介面設計要點，供實作時參考。

```fortran
! data_types.f90
module data_types
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  public :: Block
  type :: Block
     integer :: n = 0
     real(8), allocatable :: data(:)
  end type Block
end module data_types

! fileio.f90
module fileio
  use data_types
  implicit none
  public :: open_files, read_block, close_files
contains
  subroutine open_files(paths, units, nfiles)
    character(len=*), intent(in) :: paths(:)
    integer, intent(out) :: units(:)
    integer, intent(out) :: nfiles
    ! 实作：開檔與單位分配
  end subroutine open_files

  subroutine read_block(unit, blk, ok, eof)
    integer, intent(in) :: unit
    type(Block), intent(out) :: blk
    integer, intent(out) :: ok
    logical, intent(out) :: eof
    ! 实作：從檔案讀取一個區塊
  end subroutine read_block

  subroutine close_files(units, nfiles)
    integer, intent(in) :: units(:)
    integer, intent(in) :: nfiles
    ! 实作：關閉檔案
  end subroutine close_files
end module fileio

! merger.f90
module merger
  use data_types
  implicit none
  public :: merge_and_stats
contains
  subroutine merge_and_stats(blocks, k, N_total, mean)
    type(Block), intent(in) :: blocks(:)
    integer, intent(in) :: k
    integer, intent(out) :: N_total
    real(8), intent(out) :: mean
    ! 實作：合併多個區塊並計算總數與平均值
    integer :: i, j, count
    real(8) :: sum

    N_total = 0
    sum = 0.0_8
    do i = 1, k
      N_total = N_total + blocks(i)%n
      do j = 1, blocks(i)%n
        sum = sum + blocks(i)%data(j)
      end do
    end do
    if (N_total > 0) mean = sum / real(N_total, 8) else mean = 0.0_8
  end subroutine merge_and_stats
end module merger
```

說明
- 屬性 Block 與公開介面以 data_types 與 fileio 兩模組分離，實作時可用不同的檔案來源作為 block 的資料。合併函式 merge_and_stats 以簡單的呼叫方式計算 N_total 與 mean，支援單元測試的輸入產生。

結語與過渡
透過上述模組化設計與範例骨架，已建立一個具備穩健檔案 I/O 流程與容錯路徑的開發藍圖，便於補充單元測試與模擬場景。下一節將聚焦於輸入檢核與容錯設計的實作要點，並展示如何將這些元件整合為更完整的科學計算介面，提升整體魯棒性與可維護性。接著，我們將進一步分析各模組的測試策略與實作細節，實作出一套可用於實務專案的測試框架。

### 節2：單元測試與驗證

本節聚焦單元測試與驗證的實作要點，藉由模組化設計提升測試的可重複性與維護性。對於計算 N_total 與 mean 的核心演算法，我們需要不只驗證正確數值，亦要檢視空輸入、極端數值與長度變化等情境。單元測試的核心原則是把邏輯與 I/O 脫鉤，透過可預測的輸入產生與清晰的斷言，快速回饋開發者的修改是否破壞既有行為。

- 測試設計原則
  - 將核心計算寫成純函式/子程式，降低副作用，易於在單元測試中重現。
  - 使用可控的測試輸入產生模組，確保測試案例與資料穩定且可重複。
  - 建立簡單的斷言機制，對比實際輸出與預期值，允許設定誤差容忍度。

- 核心數學與驗證點
  - 公式關係：
  $$N_{\text{total}} = \text{size}(x) $$
  $$ \text{mean} = \frac{\sum_i x_i}{N_{\text{total}}} $$
  。若 $N_{\text{total}}=0$，應定義適當的回傳值以避免未定義狀況。
  - 流程上需涵蓋：一般情境、負數與浮點數混合、空陣列、以及長陣列的表現。

- 輸入產生與測試骨架
  - 建立獨立模組產生測試用的資料陣列，避免測試案例彼此干擾。
  - 測試框架以簡易的 assert 機制實作，便於快速擴充新案例。

以下為實作範例，分成三部：核心演算法模組、測試輸入產生模組、以及測試檢驗程式。

```fortran
! 1) 核心演算法模組：計算 N_total 與 mean
module stats_module
contains
  subroutine compute_N_and_mean(data, N_total, mean)
    real, intent(in)  :: data(:)
    integer, intent(out) :: N_total
    real, intent(out)    :: mean

    integer :: n
    real    :: s

    n = size(data)
    N_total = n
    if (n > 0) then
       s = sum(data)
       mean = s / real(n)
    else
       mean = 0.0
    end if
  end subroutine compute_N_and_mean
end module stats_module
```

```fortran
! 2) 測試輸入產生模組
module test_inputs
contains
  subroutine gen_case1(arr)
    real, allocatable, intent(out) :: arr(:)
    allocate(arr(3))
    arr = (/ 1.0, 2.0, 3.0 /)
  end subroutine

  subroutine gen_case2(arr)
    real, allocatable, intent(out) :: arr(:)
    allocate(arr(4))
    arr = (/ 0.0, -1.0, 4.0, 5.0 /)
  end subroutine

  subroutine gen_case3(arr)
    real, allocatable, intent(out) :: arr(:)
    allocate(arr(0))
  end subroutine
end module test_inputs
```

```fortran
! 3) 測試執行與驗證
program test_stats
  use stats_module
  use test_inputs
  implicit none

  real, allocatable :: data(:)
  integer :: N
  real    :: mean
  logical :: ok

  ! Case 1: [1,2,3] -> N=3, mean=2.0
  call gen_case1(data)
  call compute_N_and_mean(data, N, mean)
  print *, "Case1: N=", N, " mean=", mean
  if (N == size(data) .and. abs(mean - 2.0) < 1.0e-12) then
     print *, "Case1 passed"
  else
     print *, "Case1 failed"
  end if

  ! Case 2: [0, -1, 4, 5] -> N=4, mean=2.0
  call gen_case2(data)
  call compute_N_and_mean(data, N, mean)
  print *, "Case2: N=", N, " mean=", mean
  if (N == size(data) .and. abs(mean - 2.0) < 1.0e-12) then
     print *, "Case2 passed"
  else
     print *, "Case2 failed"
  end if

  ! Case 3: empty array -> N=0, mean=0.0
  call gen_case3(data)
  call compute_N_and_mean(data, N, mean)
  print *, "Case3: N=", N, " mean=", mean
  if (N == 0 .and. abs(mean - 0.0) < 1.0e-12) then
     print *, "Case3 passed"
  else
     print *, "Case3 failed"
  end if
end program test_stats
```

- 測試結果的意義在於能快速回報功能是否在常見與邊界情境下保持穩健。若未達預期，可透過檢視輸入產生模組與核心演算法，逐步定位穩定性瓶頸。

結語與過渡
透過上述模組化設計與簡易測試框架，已建立一個可擴充的單元驗證基礎，便於加入更多案例與模擬情境。下一節將聚焦於輸入檢核與容錯設計的實作要點，並展示如何將這些元件整合為更完整的科學計算介面，提升整體魯棒性與可維護性。這樣的驗證機制也為實務專案的測試框架奠定穩固基礎。

### 節3：除錯工具與排錯流程

在實務專案中，除錯不只是找出「為什麼不對」，更重要的是建立可重現的排錯流程，讓團隊能快速定位穩定性瓶頸並回歸到核心演算法的正確性。以下內容以 FORTRAN 為例，說明常見工具、流程與實作要點，並提供可操作的範例。

- 工具與環境要點
  - 編譯與除錯：以 gfortran 為例，配合 -g、-O0 與 -fcheck=all 等選項，可在邊界條件與輸入檢查上捕捉錯誤。常見旗標包括 -Wall 與 -Wextra 提醒潛在問題，-fbacktrace 方便追蹤堆疊。顯示性日誌與錯誤碼有助於快速定位區塊。
  - 除錯介面：gdb 或 DDD 等 GUI 前端可在關鍵子程序設定斷點、觀察變數、檢查陣列界線。Fortran 的邊界檢查在 -fcheck=all 開啟時尤為重要，能在越界存取時立即中斷並顯示警告。
  - 診斷方法：結合 printf/yield 日誌、單元測試框架（如 pFUnit）以及覆蓋率分析工具 gcov，能以系統性的方式驗證各模組行為與回歸。
  - 容錯與日誌設計：在模組間傳遞狀態碼與錯誤訊息，避免直接崩潰。例如，用整數狀態碼表示成功/失敗，並提供可攜帶的訊息字串，以利自動化回歸測試。

- 排錯流程（實務做法）
  - 重現與最小案例：先建立可重現的最小案例，排除外部依賴，確保問題可控。
  - 初步分析：閱讀錯誤訊息、返回碼與日誌，建立假設（例如資料長度不符、索引越界、指標/指針錯誤）。
  - 設定斷點與監視：在輸入介面、資料轉換、迴圈邊界與關鍵演算法區塊設定斷點，逐步執行並觀察變數變化。
  - 邊界條件與穩定性檢查：針對 1、0、負值、極端長度等極端情境測試，必要時開啟 -fcheck=all 以觸發額外檢查。
  - 假設驗證與回歸測試：設計單元測試案例覆蓋常見與邊界情境，並於修正後執行完整回歸測試，確保不影響其他模組。
  - 記錄與知識分享：將問題原因、修正步驟與性能影響記錄於版本控制的問題追蹤系統，方便日後參考與跨團隊協作。

- FORTRAN 的除錯實作範例
  為了呈現零基礎觀點下的實作要點，下面的範例示範如何利用 0 基的陣列界限，以及在編譯時啟動邊界檢查來捕捉錯誤。此處的意圖是讓讀者理解「定位問題的流程」，而非提供完整版解法。

  ```fortran
  ! demo_debug01.f90
  program demo_debug01
    implicit none
    integer, parameter :: n = 5
    real(8), allocatable :: a(:)
    integer :: i

    allocate(a(0:n-1))
    do i = 0, n-1
       a(i) = real(i)
    end do

    ! 故意在邊界外存取，觀察 -fcheck=all 的行為
    print *, 'a(0)=', a(0)
    print *, 'a(5)=', a(5)   ! 觸發越界
  end program demo_debug01
  ```

  編譯與執行時，搭配嚴格檢查：
  ```bash
  # 編譯與除錯
  gfortran -g -O0 -fcheck=all -fbacktrace -Wall demo_debug01.f90 -o demo_debug01
  ```
  這樣在執行到 a(5) 時若觸發越界，會直接中斷並顯示對應的檢查訊息，協助定位問題的根源。

- 日誌與容錯的設計要點
  - 錯誤分級：對於可預期的錯誤，使用狀態碼和可讀字串回傳；對於不可預期的例外，保留堆疊回溯與日誌輸出，方便追蹤。
  - 非破壞性診斷：優先在不改變核心演算法前提下，通過輸入檢核、日誌與斷點定位問題；必要時再做演算法修正。
  - 單元與整合測試：透過小型測試案例驗證每個模組行為，並透過整合測試確保模組間介面穩定性。

- 版本、可重現性與最佳實踐
  - 將編譯器版本、執行環境與依賴鎖定在版本控制中，建立可重現的建置流程。以「再現性」為核心，避免環境差異造成的偽陰性/偽陽性結果。
  - 針對邊界與隱藏條件，建立可重現的測試資料集與隨機種子設定，使結果穩健可比。

結語與過渡
透過上述工具與流程，已建立可重現的除錯與單元驗證基礎。下一節將聚焦於輸入檢核與容錯設計的實作要點，並展示如何把這些元件整合為更完整的科學計算介面，提升整體魯棒性與可維護性。如此的驗證機制也為實務專案的測試框架奠定穩固基礎。

### 節4：效能分析與優化方法

在以往的可重現除錯與單元驗證基礎之上，效能分析師的任務是找出熱點、量化瓶頸，並以可重現的流程實作優化。本文以 Roofline 模型與 Fortran 的特性為核心，說明如何在可控環境中產出穩健的效能數據，並給出實作範例。

- 基本概念
  - 計算與記憶體之間的平衡點，稱作 arithmetic intensity $I$，定義為 $I=\frac{\text{FLOPs}}{\text{Bytes transferred}}$。若 I 越高，系統越可能成為 compute-bound；若 I 較低，則 memory-bound 為主。顯示式模型可寫為 $P=\min\left(P_{\text{peak}},\, B \cdot I\right)$，其中 $P$ 是實際可達的浮點運算速率，$P_{\text{peak}}$ 為處理器的峰值浮點效能，$B$ 是記憶體頻寬。
  - Fortran 的記憶體佈局為欄主序（column-major），因此要讓迴圈在記憶體上連續存取以提高快取命中率。理想的內層迴圈應以第一維度為主（快速變化），以利存取 A(:,j) 的連續區段。
- 室內流程
  - 1) 設定可重現的測量環境：固定輸入、穩定的隨機種子、禁用 DVFS 與其他影響時脈的設定，於同一機器上執行多次取平均。
  - 2) 選取測量工具與指標：計時（wall clock、CPU time）、快取缺失、分支指令、向量化等事件。常見工具包括 perf、gprof、VTune、PAPI；以及內建的 CPU_TIME/system_clock。
  - 3) 識別熱點與瓶頸：以簡單的 micro-benchmark 與實務程式混合測量，區分 compute-bound 與 memory-bound，並計算 I 值與實際效能差距。
  - 4) 推動優化策略：調整記憶體佈局、結構化平行與向量化、迴圈重新排序與分塊（blocking），搭配編譯器旗標與 OpenMP。
- 與實作的約定
  - 所有基準都建立在同一個測試資料集與隨機種子，並記錄編譯選項、核心數、OS 版本等可重現因素。每次改動都要產出獨立的比較報告，以避免偽陰性/偽陽性。

- 具體實作案例
  下面以一個簡單的矩陣向量乘法作為示例，呈現 baseline 與優化後的版本。內層 loop 以 i 為主，確保對 A(:,j) 的連續存取。

Baseline

```fortran
! matvec_baseline.f90
subroutine matvec(m, n, A, x, y)
  integer, intent(in) :: m, n
  double precision, intent(in) :: A(m,n), x(n)
  double precision, intent(inout) :: y(m)
  integer :: i, j

  y = 0.0d0
  do j = 1, n
     do i = 1, m
        y(i) = y(i) + A(i, j) * x(j)
     end do
  end do
end subroutine
```

Optimized with cache-friendly、向量化與 OpenMP

```fortran
! matvec_optimized.f90
subroutine matvec_opt(m, n, A, x, y)
  integer, intent(in) :: m, n
  double precision, intent(in) :: A(m,n), x(n)
  double precision, intent(inout) :: y(m)
  integer :: i, j
  double precision :: xj

  y = 0.0d0

  !$omp parallel do private(i,j,xj) shared(A,x,y)
  do j = 1, n
     xj = x(j)
     do i = 1, m
        y(i) = y(i) + A(i, j) * xj
     end do
  end do
  !$omp end parallel do
end subroutine
```

- 設置與測量
  - 編譯（以 gfortran 為例，開啟 OpenMP 與最佳化）：  
    ```bash
    gfortran -O3 -fopenmp -o matvec_opt matvec_opt.f90
    ```
  - 以 perf 觀測（記憶體帶寬、指令、快取等事件）與時間：  
    ```bash
    perf stat -e cache-misses,cycles,instructions ./matvec_opt
    /usr/bin/time -f "%e" ./matvec_opt
    ```
  - 評估改進：Speedup 約為 $\frac{T_{old}}{T_{new}}$，並以 I 值與 Roofline 觀察是否向上攀升。

- 進階策略
  - 迴圈再排序與分塊（blocking）：針對更大尺寸的 A，可以在 j 維度做區塊化，減少快取抖動。
  - 向量化與 Do concurrent：使用 do concurrent、或編譯器自動向量化提示，提升單位時間的 FLOPs。
  - OpenMP 與多核心：在不影響程式正確性的前提下，分派工作到不同核心，並注意維護 y 的寫入競爭。
  - 編譯器旗標與硬體特性：-O3、-funroll-loops、-march=native，以及 -fopenmp 以發揮平行與向量化能力。

- 小結與量測表徵
  - 真實世界的效能提升往往來自於多次微小修正的累積，核心在於穩健的測量與可追溯的比較。透過 I 值與 Roofline 的框架，可以評估改動是否真正提升了計算效率，而非僅僅減少了某一個指標的數值。

- 過渡與展望
  透過上述分析與實作，已建構穩健的效能評估與初步優化流程。下一節將聚焦於輸入檢核與容錯設計的實作要點，並展示如何把這些效能元件整合為更完整的科學計算介面，以提升整體魯棒性與可維護性。

### 節5：文件化與版本控制的實踫

在前述的效能評估與最小變更實作之後，文件化與版本控制成為穩健開發流程的中樞。良好的文件能讓新同仁快速上手，讓結果具可重現性；嚴謹的版本控制則讓每一次修正、每一次實驗都可追溯、可比較，進而支撐長期的科學計算專案。

- 文件化的核心要點
  - 程式註解與外部文件並行：Fortran 的模組與介面需配有清晰的註解，並以 Doxygen 等工具生成 API 文件。Fortran 常見的註解風格是使用註解行，以反斜線開頭，如
    ```fortran
    ! \brief 計算向量平均值
    ! \param arr 輸入的一維 real 向量
    ! \return 平均值
    ```
    以利自動產出說明文件與 API 摘要。
  - 設計決策日誌與測試報告：在專案中保留 DESIGN.md 與 DECISIONS.md，記錄為何選用某種演算法、何時放棄某些最佳化，以及對應的 I 值與 Roofline 之測試結果。必要時嵌入公式以方便回溯：
  $$AI = \frac{\text{FLOPs}}{\text{Bytes moved}}$$
  $$P=\min(\text{FLOPS}_{\text{peak}}, AI \cdot \text{BW}_{\text{peak}})$$
  - CHANGELOG 和版本說明：每天的更動皆在 CHANGELOG.md 逐條記錄，包含修正的 bug、性能改動、API 變更與相容性說明，便於日後審核與成果對比。

- 版本控制的實務
  - 建立清晰的工作流：採用 feature 分支與 pull request 的模式，分支命名如 feature/mean-enhancement、fix/robust-input，避免直接在 main/master 上開發。提交訊息應規範化，如
    - feat(api): add mean() with doc comments
    - fix(input): handle NaN inputs gracefully
    - docs: update API说明與測試結果
  - 保留可重現的環境：記錄編譯器版本、旗標、外部庫版本，並可在 CI 內重現。若使用 Fortran 專案管理工具，如 fpm，應同時提交 fpm.toml 的版本約束與環境設定。
  - 測試與結果的版本化：測試資料與測試結果存放於 tests/ 目錄，關鍵測試的結果（包括性能測試）以 artifacts 形式在 CI 產出，並將對比結果寫入報告檔案。若需要長期追蹤， consider 使用 Git LFS 存放大型資料檔案，或將資料託管在獨立資料倉庫。

- Fortran 專屬的工作樣本
  - 代碼註解與文檔範例（Fortran 模組註解）
    ```fortran
    module stats
      implicit none
    contains
      ! \brief Compute mean of a 1-D real array
      ! \param arr Input 1-D real array
      ! \return Mean value
      function mean(arr) result(m)
        real, intent(in) :: arr(:)
        real :: m
        m = sum(arr) / size(arr)
      end function mean
    end module stats
    ```
  - 建置與專案檔案（fpm.toml）範例
    ```toml
    [project]
    name = "fortran-demo"
    version = "0.1.0"
    authors = ["作者 <author@example.com>"]

    [[targets]]
    name = "demo"
    path = "src/demo.f90"
    ```
  - 版本控制與 CI 流程的簡要示意
    ```md
    1. 新功能分支 feature/mean-enhancement
    2. 本地跑測試與性能對比
    3. 提交 message 規範 e.g. feat(api): add mean() with docs
    4. 提交至遠端，創建 PR，CI 自動編譯與測試
    5. 合併至 main，更新 CHANGELOG 與版本號
    ```

- 自動化與可持續性
  - 使用文檔生成與測試自動化：在 CI 中自動產出 API 文件、執行單元測試、並產生性能對比報告。若有 Roofline 或 I 值的測試，將結果以床列檔案或報告形式輸出，方便跨版本比較。
  - 重現性與可追蹤性：把關鍵運算的參數與資料流清楚記錄，避免因環境不同而導致結果不可重現。每次性能優化前後，務必以同一套資料與相同編譯旗標執行，並在報告中附上比較圖與公式。

- 整合長效觀察
  - 將文件、變更、測試與結果的版本控制視為科學計算介面的核心支柱，讓專案在日後的維護、再產生與跨團隊協作時，都能快速定位、比對與重現。

結束本節時，請記得將本次修改的實驗與文檔變更整理成清晰的提交與報告，以便在下一次迭代中快速落地。下一節將聚焦於輸入檢核與容錯設計的實作要點，並展示如何把這些效能與可維護性元件整合為更完整的科學計算介面。

