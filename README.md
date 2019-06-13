# Water_-consumption_Haskell

This Haskell program should allow the emission of water consumption invoices. Every month are counted the cubic meters of water consumption
knowing that from 0 to 5 meters the liter of water is paid at 50 cents (0.5
euros) but from 6 to 15 meters is charged the value of 1 euro per liter. In
addition to the cost directly associated with water consumption, a sanitation fee is paid depending on the number of persons who live in the house,
with a charge of 1.5 euros per person per month.
type Consumption = (IDClient, Name, Address, NPersons, Cubicmeters0_5, Cubicmeters6_15, Month, Year).
type Invoice = (IDClient, Water_bill, Month, Year).
In this case, it is intended to develop a program that allows a set of operations:
1. insert the consumption of one client in one month;
2. create the invoice of a client in Latex format based on the consumption
of one month;
3. calculate the average consumption for one client in one year;
4. calculate the total payment for one client at the end of one year;
5. total number of cubicmeters consummated over 5 cubicmeters of all
clients in the last year;
6. generate a graph connecting the monthly payment of a client during
one year and the size of each node must be proportional to that payment value.
