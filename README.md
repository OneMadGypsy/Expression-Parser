# Expression-Parser
A regular expression based math expression parser for the haxe language

Simply drop in the top-level of your source folder.

example usage:

var expr:String = "sin(random * pi)";
var e:Float = Expression.eval(expr);
if(! Math.isNaN(e) )	
{... //safe
