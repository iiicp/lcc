preprocessing-file:    
&emsp; &emsp; group<sub>opt</sub>

group:    
&emsp; &emsp; group-part    
&emsp; &emsp; group group-part

group-part:     
&emsp; &emsp; if-section  
&emsp; &emsp; control-line     
&emsp; &emsp; text-line     
&emsp; &emsp; **#** non-directive  

if-section:        
&emsp; &emsp; if-group elif-groups<sub>opt</sub> else-group<sub>opt</sub> endif-line

if-group:   
&emsp; &emsp; **# if** constant-expression new-line group<sub>opt</sub>    
&emsp; &emsp; **# ifdef** identifier new-line group<sub>opt</sub>    
&emsp; &emsp; **# ifndef** identifier new-line group<sub>opt</sub>

elif-groups:   
&emsp; &emsp; elif-group    
&emsp; &emsp; elif-groups elif-group    


elif-group:     
&emsp; &emsp; **# elif** constant-expression  new-line  group<sub>opt</sub>   

else-group:   
&emsp; &emsp; **# else** new-line group<sub>opt</sub>    

endif-line:   
&emsp; &emsp; **# endif** new-line

control-line:   
&emsp; &emsp; **# include** pp-tokens new-line    
&emsp; &emsp; **# define** identifier replacement-list new-line    
&emsp; &emsp; **# define** identifier lparen identifier-list<sub>opt</sub> **)** replacement-list new-line   
&emsp; &emsp; **# define** identifier lparen ... **)** replacement-list new-line    
&emsp; &emsp; **# define** identifier lparen identifier-list **, ... )** replacement-list new-line    
&emsp; &emsp; **# undef** identifier new-line   
&emsp; &emsp; **# line** pp-tokens new-line  
&emsp; &emsp; **# error** pp-tokens<sub>opt</sub> new-line   
&emsp; &emsp; **# pragma** pp-tokens<sub>opt</sub> new-line    
&emsp; &emsp; **#** &emsp; &emsp; &emsp; &emsp; new-line

text-line:    
&emsp; &emsp; pp-tokens<sub>opt</sub>  new-line  

non-directive:   
&emsp; &emsp; pp-tokens new-line  

lparen:     
&emsp; &emsp; a **(** character not immediately preceded by white-space   

replacement-list:   
&emsp; &emsp; pp-tokens<sub>opt</sub>  

pp-tokens:    
&emsp; &emsp; preprocessing-token   
&emsp; &emsp; pp-tokens preprocessing-token  

new-line:     
&emsp; &emsp; the new-line character    