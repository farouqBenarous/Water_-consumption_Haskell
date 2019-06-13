import System.Info
import Data.Char
--------------------------- Invoice  -------------------------------------------------------

data Invoice = Invoice String String String  String
listinv :: [Invoice]
listinv=[]

list=[]

loadTab_invoice = do 
     s <-readFile "invoice.txt"
     return (gerlist_invoice (map words (lines s)))

gerlist_invoice [] = []
gerlist_invoice ([a,b,c,d]:xs) = (a,b,c,d):(gerlist_invoice xs)

print_invoice [] =""
print_invoice ((a,b,c,d):xs) = "a" ++ a ++ "b" ++ b ++ "c" ++ c ++ "d" ++ d  ++  "\n" ++ (print_invoice xs)

search_invoice id [] = ""
search_invoice id ((i,w,m,y):xs) | id==i = w
                                 | id/=i = search_invoice id xs
--------------------------- Invoice  -------------------------------------------------------

--------------------------- Consumption  -------------------------------------------------------
data Consumption = Consumption String String String String String String String String
listcons :: [Consumption]
listcons=[]

loadTab_Consumption = do 
     s <-readFile "consumption.txt"
     return (gerlist_consumption (map words (lines s)))

gerlist_consumption [] = []
gerlist_consumption ([a,b,c,d,e,f,g,h]:xs) = (a,b,c,d,e,f,g,h):(gerlist_consumption xs)

print_consump [] =""
print_consump ((a,b,c,d,e,f,g,h):xs) = "a" ++ a ++ "b" ++ b ++ "c" ++ c ++ "d" ++ d ++ "e" ++ e ++ "f" ++ f ++ "g" ++ g ++ "h" ++ h ++  "\n" ++ (print_consump xs)
--------------------------- Consumption  -------------------------------------------------------


------------------  1 .Insert new consumption ---------------------------------------------------------
insert_a_new_consumption :: IO()
insert_a_new_consumption = do 
            print ("Set your Id")  
            i<- getLine
            print ("Set your name")
            n<- getLine
            print ("Set your address")
            a<- getLine
            print ("Set your number_of_Persons")
            np<- getLine
            print ("Set your cubicmeters6_15")
            cubis6_15 <- getLine
            print ("Set your cubicmeters0_5")
            cubis0_5<- getLine
            print ("Set your month")    
            m <- getLine
            print ("Set your year")
            y <- getLine
            let cubi05 = read cubis0_5 :: Float
            let cubi615= read cubis6_15 :: Float
            let pn = read np :: Float             
            let w =  water_total_bill cubi05  cubi615 pn
            let w1 = show (w) 
            appendFile "consumption.txt" (i ++ "\t" ++ n ++ "\t" ++ a ++ "\t" ++ np ++ "\t" ++ cubis6_15 ++ "\t" ++ cubis0_5 ++ "\t" ++ m ++"\t" ++y ++ "\n")
            appendFile "invoice.txt" (i ++"\t" ++ w1 ++ "\t" ++ m ++ "\t" ++ y  ++ "\n")
            print ("Insert another one ? y/n" ) 
            resp <- getLine
            if (resp=="y" || resp=="Y") then insert_a_new_consumption   else return()


bill_for_cubicmeter0_5  a = do 
     if (a>5 ) then a* 1000 * 0.5 else  a 

bill_for_cubicmeter6_15 a = do 
     a *100      

bill_for_sanitation a = do
   a * 1.5

water_total_bill a b c =  do 
     let a1 = bill_for_cubicmeter0_5 a 
     let b1 = bill_for_cubicmeter6_15 b 
     let c1 = bill_for_sanitation c  
     a1+b1+c1
------------------ 1 . Insert new consumption ---------------------------------------------------------




------------------- 2 .Create Latex Format ------------------------------------------------------------
create_Layex_format  = do
     print ("Set your Id")   
     i<- getLine
     print ("Set the month ")
     m<- getLine
     print ("Set the year ")
     y<- getLine
     print ("Set the adress   ")
     a<- getLine
     print ("Set the name  ")
     n<- getLine

     let i1 = show(i)
     let m1 = show (m)
     let y1 = show (y)
     let a1 = show (a)
     let n1 = show (n)
     list <- loadTab_invoice  
     let x =search_invoice i  list
    
     
     appendFile "latex.tex" ("/documentclass{article}" ++ "\n" ++ 
          "/usepackage{graphicx}" ++ "\n" ++"/graphicspath{ {./images/} }" ++ "\n" ++ 
          "/usepackage[utf8]{inputenc}" ++ "\n" ++ "/usepackage[T1]{fontenc}" ++ "\n" ++
           "/usepackage{hyperref}" ++ "\n" ++ "/usepackage{url}" ++ "\n"  ++ "/usepackage{booktabs}" ++ "\n" 
           ++ "/title{Invoice of Water Consumption}" ++"\n" ++"/date{May 2019}"++ "\n" ++ "/author{Benarous Ahmed Omar Farouk }" 
           ++ "\n"++ "/begin{document}" ++ "\n"++ "/maketitle" ++"\n" ++ "/begin{abstract}" ++ "\n"
           ++ "This invoice is generated using the latex format in the prolog language" ++ "\n"
           ++ "/end{abstract}" ++ "\n" ++ "/keywords {Datascience /and Programming paradims /and Water Consumption /and haskell}"
           ++ "\n" ++ "/section{Water Invoice}" ++ "\n" ++ "Client ID : " ++ i1 ++"\n" ++ "Month Of the bill "++ m1
           ++ "\n" ++ "Year of the bill : "++ y1 ++"\n" ++"Adress of the client : "++ a1 ++ "\n" ++"Name Of Client : " ++ n1
           ++ "\n" ++ "Bills of client : " ++ x ++ "\n" ++ "/end{document}" )
     -- so here i have to create a function that takes as  an argument id client and it will return 

     print("Done ")

       

     

------------------- 2 .Create Latex Format ------------------------------------------------------------



--------------------  3 .Check average consuption a year -----------------------------------------------
average_of_both_consumption  = do
     print("Enter Client Id : ")
     i<- getLine
     print("Enter the year you want to check average for : ")
     y<- getLine 
     list <- loadTab_Consumption
     let x = search_consumption i y list
     print("Average Consumption of the year ")
     print(i)
     print(" is ")
     print(x/12)
     print("cubicmeters.")

 
search_consumption id ye [] = 0
search_consumption id ye ((i , n , a , np , cubis6_15 , cubis0_5 , m , y):xs) = 
     if (id==i && ye ==y  ) then 0+(read (cubis0_5) ::Float) +(read (cubis6_15) ::Float)  + search_consumption id ye xs  else search_consumption id ye xs
-------------------- 3. Check average consuption a year -----------------------------------------------



-----------------------------4 .Check total payment made in an year ------------------------------
total_payment = do 
     print("Enter Client Id : ")
     i<- getLine
     print("Enter the year you want to check average for : ")
     y<- getLine 
     list <- loadTab_invoice
     let x = search_payment i y list 
     print("total of payment made in the year ")
     print(y)
     print("is ")
     print(x)

search_payment i y [] = 0
search_payment i y ((id,w,m,ye) :xs) = 
     if (id == i && y==ye) then 0+(read (w) ::Float) + search_payment i y xs else search_payment i y xs  
-----------------------------4 .Check total payment made in an year ------------------------------





------------------ 5.  Check over 5 cubicmeter consumption by all clients in a year --------
  
check_over_5 = do
     print("Enter the year ")
     y <- getLine
     list <- loadTab_Consumption
     let x = check_over_5_all y list
     print ("the result is : ")
     print(x)

check_over_5_all ye [] = 0
check_over_5_all ye ((i , n , a , np , cubis6_15 , cubis0_5 , m , y):xs) =  
     if (ye==y && cubis0_5 > "5" ) then  0 + (read (cubis0_5)::Float)  + check_over_5_all  ye xs  else check_over_5_all  ye xs

------------------ 5.  Check over 5 cubicmeter consumption by all clients in a year --------



main :: IO()
main = do listcons <- loadTab_Consumption
          putStr(print_consump listcons)
          listinv <- loadTab_invoice                 
          putStr(print_invoice listinv)
          print " +----------------------------------------------------------------------+ \n "
          print "' | >> 1. Insert a new consumption                                       | '"
          print "' | >> 2. Create the Latex format invoice                                | ' "
          print "' | >> 3. Check average consumption of a year                            | '"
          print "' | >> 4. Check total payment made in an year                            | '"
          print "' | >> 5. Check over 5 cubicmeter consumption by all clients in a year   | '"
          print "' | >> 6. Check the graph                                                | '"
          resp <- getLine
          if (resp=="1") then do insert_a_new_consumption
          else if (resp=="2") then do create_Layex_format
          else if (resp=="3") then do average_of_both_consumption     
          else if (resp=="4") then do total_payment
          else if (resp=="5") then do check_over_5     
          else if (resp=="6") then do gengraph else return()

----------------------------Graph -----------------------------------------------
gengraph  = do
     print("Enter Client Id : ")
     i<- getLine
     print("Enter the year you want to check average for : ")
     y<- getLine 
     list <- loadTab_invoice
     let jan1 = get_jan i y list
     let jan = show (jan1)
     
     let feb1 = get_feb i y list
     let feb = show(feb1)

     let mar1 = get_mar i y list
     let mar = show(mar1)

     let apr1 = get_apri i y list
     let apr = show(apr1) 
     
     let may1 = get_may  i y list 
     let may = show(may1)
     
     let juin1 = get_juin i y list
     let juin = show(juin1)

     let julay1 = get_julay i y list
     let julay = show(julay1)

     let augu1 = get_augu i y list 
     let augu = show (augu1)

     let sep1 = get_sep i y list
     let sep = show(sep1)

     let oct1 = get_oct i y list
     let oct = show(oct1)

     let nov1 = get_nov  i y list 
     let nov = show(nov1)
    
     let dec1= get_dec i y list
     let dec = show (dec1)


     appendFile "fich.dot" ("Digraph g { rankdir = LR ;node [style=filled]; "++ "January [lable ='January' ,height ="++ jan ++"]" ++
           "February [lable ='February' ,height ="++  feb  ++"]"  ++  "March [lable ='March' ,height ="++ mar ++"]"  ++
           "April [lable ='April' ,height ="++apr ++"]" ++  "May [lable ='May' ,height ="++may++"]" ++ "June [lable ='Jun' ,height ="++ juin ++"]"++  
           "July [lable ='July' ,height =" ++julay ++"]" ++ "August [lable ='August' ,height ="++ augu ++"]"  ++ "September [lable ='September' ,height ="++ sep ++"]"  ++
           "October [lable ='October' ,height =" ++oct ++"]"  ++"November [lable ='November' ,height =" ++oct ++"]" ++"December [lable ='December' ,height ="++ dec ++"]" ++ "}"  )




get_jan i y [] = 0
get_jan i y ((id,w,m,ye) :xs) = 
     if (id == i && y==ye && m == "1") then 0+(read (w) ::Float) + get_jan i y xs else get_jan i y xs  

get_feb i y [] = 0
get_feb i y ((id,w,m,ye) :xs) = 
     if (id == i && y==ye && m == "2") then 0+(read (w) ::Float) + get_feb i y xs else get_feb i y xs  

get_mar i y [] = 0
get_mar i y ((id,w,m,ye) :xs) = 
     if (id == i && y==ye && m == "3") then 0+(read (w) ::Float) + get_mar i y xs else get_mar i y xs  

get_apri i y [] = 0
get_apri i y ((id,w,m,ye) :xs) = 
     if (id == i && y==ye && m =="4") then 0+(read (w) ::Float) + get_apri i y xs else get_apri i y xs  


get_may i y [] = 0
get_may i y ((id,w,m,ye) :xs) = 
     if (id == i && y==ye && m =="5") then 0+(read (w) ::Float) + get_may i y xs else get_may i y xs  


get_juin i y [] = 0
get_juin i y ((id,w,m,ye) :xs) = 
     if (id == i && y==ye && m =="6") then 0+(read (w) ::Float) + get_juin i y xs else get_juin i y xs  


get_julay i y [] = 0
get_julay i y ((id,w,m,ye) :xs) = 
     if (id == i && y==ye && m == "7") then 0+(read (w) ::Float) + get_julay i y xs else get_julay i y xs  


get_augu i y [] = 0
get_augu i y ((id,w,m,ye) :xs) = 
     if (id == i && y==ye && m == "8") then 0+(read (w) ::Float) + get_augu i y xs else get_augu i y xs  


get_sep i y [] = 0
get_sep i y ((id,w,m,ye) :xs) = 
     if (id == i && y==ye && m == "9") then 0+(read (w) ::Float) + get_sep i y xs else get_sep i y xs  

get_oct i y [] = 0
get_oct i y ((id,w,m,ye) :xs) = 
     if (id == i && y==ye && m == "10") then 0+(read (w) ::Float) + get_oct i y xs else get_oct i y xs  

get_nov i y [] = 0
get_nov i y ((id,w,m,ye) :xs) = 
     if (id == i && y==ye && m =="11") then 0+(read (w) ::Float) + get_nov i y xs else get_nov i y xs  

get_dec i y [] = 0
get_dec i y ((id,w,m,ye) :xs) = 
     if (id == i && y==ye && m == "12") then 0+(read (w) ::Float) + get_dec i y xs else get_dec i y xs  





-----------------------------Graph -----------------------------------------------
