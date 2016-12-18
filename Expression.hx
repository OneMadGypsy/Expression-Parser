package;

/**	Expression.hx - a RegEx parser for stringified math expressions
 * @version	2.2
 * @author	OneMadGypsy - 11/25/2016-22:25cn
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain this list of conditions 
 * 	   and the following disclaimer.
 * 
 * THIS SOFTWARE IS PROVIDED BY ONEMADGYPSY "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE ONEMADGYPSY BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 * 
 * @example 
 * 	var expr:String = "sin(random * pi)";
 * 	var e:Float = Expression.eval(expr);
 * 	if(! Math.isNaN(e) )	
 * 	{... //safe
 */

import openfl.errors.Error;

//add a little organization to my regex object
typedef Expr_t = 
{	var white:EReg;
	var parens:EReg;
	var constant:EReg;
	var funcs:EReg;
	var token:EReg;
	var toknum:EReg;
	var infix:EReg;
}

//a container to make passing this data easier
typedef Tokens_t = 
{	var expression:String;
	var stack:Array<String>;
}

class Expression
{
	//simple priority delegate
	private static var priority:Array<String>	= ["*","/","+","-"];
	
	//string constants for regex creation
	private static inline var WHITESPACE:String	= "[\\t\\r\\n\\s]";
	private static inline var PARENS:String		= "[\\(\\)]";
	private static inline var CONSTANT:String	= "PI|POSITIVE_INFINITY|NEGATIVE_INFINITY|(?<![0-9.])E";
	private static inline var FUNCTION:String	= "isNaN|isFinite|cos|acos|sin|asin|tan|atan|atan2|random|abs|ffloor|floor|fceil|ceil|fround|round|sqrt|max|min|log|pow|exp";
	private static inline var TOKEN:String		= "TOKEN[0-9]+";
	private static inline var TOKNUM:String		= "(?<=TOKEN)[0-9]+";
	private static inline var INFIX:String		= "(?<=[0-9a-z]|\\))(?<![0-9.]E)[-+/*]";
	
	//regex container
	private static var regex:Expr_t = 
	{	white		:new EReg(WHITESPACE, "g"),
		parens		:new EReg(PARENS	, "g"),
		constant	:new EReg(CONSTANT	, "i"),
		funcs		:new EReg(FUNCTION	, "i"),
		token		:new EReg(TOKEN		, "gi"),
		toknum		:new EReg(TOKNUM	, "i"),
		infix		:new EReg(INFIX		, "gi")
	};
	
	/** PUBLIC_INTERFACE
	 * @param	expr - the expression to process
	 * @return	the final result of the expression or NaN
	 */
	public static function eval(expr:String):Float
	{	
		expr = (regex.white).replace(expr, "");	//remove all whitespace
		return parse(expr);						//return recursive result
	}
	
	/**	PARSE_STRING_EXPRESSION_TO_FLOAT_RESULT
	 * 		use eval to init this function
	 * @param	expr	- the expression to parse
	 * @return	the final result
	 */
	private static function parse(expr:String):Float
	{	
		//stacks to store expressions and operators
		var exp_stack:Array<String> = new Array<String>();
		var ops_stack:Array<String> = new Array<String>();
		
		//to be a stack of exp_stack results
		var res_stack:Array<Float> = new Array<Float>();
		
		//convert parents to tokens so only operators on this level will be parsed
		var tokens = tokenize(expr);
		
		//create an array of everything except actual operators
		var exp_stack:Array<String> = (regex.infix).split(tokens.expression);
		//if by any chance there are null positions - remove
		flushNull(exp_stack);
		
		//reformat the res_stack values into a regex
		var exp:String;
		exp =  (~/\(/g).replace(exp_stack.join("|"), "\\(");
		exp =  (~/\)/g).replace(exp, "\\)");
		exp = (~/\|E/g).replace(exp, "|(?<![0-9.])E");
		
		//create an array of only actual operators
		var ops_stack:Array<String>  = (new EReg(exp, "g")).split(tokens.expression);
		//if by any chance there are null positions - remove
		flushNull(ops_stack);
		
		//helper vars for retrieving values from a token
		var inner:Null<String>, s:Int;
		
		//parse the expression stack into results
		for (r in 0...exp_stack.length)
		{	
			//if this expression is a token retrieve the associated inner data
			if (regex.toknum.match(exp_stack[r]))
			{	s = Std.parseInt(regex.toknum.matched(0));
				if (s < tokens.stack.length)
					inner = tokens.stack[s];
				else throw new Error("Expression.parse(expr:String): A token was found with no accompanying data in the stack token#"+s);
			} else inner = null;
			
			//get the results of this expression
			res_stack[r] = parseType( exp_stack[r], inner );
		}
		
		//if there are operators to apply
		if(ops_stack.length > 0)
		{	
			//do operations according to standard math priority
			for(p in 0...priority.length)
			{
				var n:Int = -1;	//prime
				
				//while there are operators to process
				while ((++n) < ops_stack.length) 
				{
					//if the current operator is of the current priority
					if (ops_stack[n] == priority[p])
					{	
						if((n+1) < res_stack.length)
						{	
							//if we have 2 valid values to work with
							if ((! Math.isNaN(res_stack[n]) ) && (! Math.isNaN(res_stack[n + 1]) ))
							{	//store the result
								res_stack[n] = value(res_stack[n], ops_stack[n], res_stack[n + 1]);
								
								//strip the appropriate values from ther stacks
								res_stack.splice(n + 1, 1);
								ops_stack.splice(n,  1);
								
								//back up and run this iteration again using the new res_stack value, next operation & next value
								--n;
							}
						}
					}
				}
			}
		}
		
		//regardless of the situation res_stack should contain the one final value
		if(res_stack.length == 1) return res_stack[0];
		else throw new Error("Expression.parse(expr:String): res_stack.length is "+res_stack.length+" should be 1");
		
		return Math.NaN;
	}
	
	/**	FLUSH_NULL_CHARACTERS_FROM_A_(SUGGESTED)_SPLIT_ARRAY
	 * @param	arr	 - the array to remove null indexes from.
	 */
	private static function flushNull(arr:Array<String>):Void
	{
		var n:Int = -1;
		while ((++n) < arr.length)
			if (arr[n].length == 0)
			{	arr.splice(n, 1);
				--n;
			}
	}
	
	/** TOKENIZE
	 * 		by converting "inner expressions" to tokens, the expression can be easily and properly split on it's operators
	 * 
	 * 		Tokens_t:
	 * 		tokens.expression	- stores a "tokened" version of the original expression
	 * 		tokens.stack 		- stores the expressions that were replaced with tokens
	 * 
	 * 		ex:
	 * 		before: (-4/7) + (sin(pi*45) / 360) * sqrt(2)
	 * 		after:	
	 * 			tokens.expression = (TOKEN0) + (TOKEN1) * sqrt(TOKEN2)
	 * 			tokens.stack[0] = -4/7
	 * 			tokens.stack[1] = sin(pi*45) / 360
	 * 			tokens.stack[2] = 2
	 * 
	 * @param	expr - the expression to tokenize
	 * @return	an instance of Tokens_t
	 */
	private static function tokenize(expr:String):Tokens_t
	{
		var cnt:Int 	=  0;
		var begin:Int	= -1;
		var end:Int 	= -1;
		
		var tokens:Tokens_t = 
		{	expression:expr.substring(0),	//make a clone to perform surgery on
			stack:[]
		};
		
		var t_expr:String, token:String;
		
		regex.parens.map(expr, function(reg:EReg):String
		{
			var result	= reg.matched(0);				//the current matched string
			var index	= reg.matchedPos().pos;			//the position of this result
			
			switch( result )
			{	
				case "(":								//if this is an opening delimiter
					++cnt;								//increment unclosed parents
					if (begin == -1) 					//if this is the first opening delimiter
						begin = index + 1;
				case ")":								//if this is a closing delimiter
					--cnt;								//decrement unclosed parents
					if ((cnt == 0) && (begin > -1))		//if there are no unclosed parents and an open delim exists this is the group close delim
						end = index;
			}
			
			//if a group has been found
			if ((begin > -1) && (end > -1) && (cnt == 0))
			{
				//create an overwrite token
				token  = "TOKEN" + tokens.stack.length;
				//store group guts
				t_expr = expr.substring(begin, end);
				//push guts onto the token stack
				tokens.stack.push(t_expr);
				//replace the guts with a token
				tokens.expression = StringTools.replace(tokens.expression, t_expr, token);
				//reset this condition so the next group can be found
				begin = -1;
			}
			
			//to satisfy the expected condition. value is unchanged
			return result;
		});
		
		//return a token container of parsed groups
		return tokens;
	}
	
	/** DETOKENIZE_PART_OF_A_TOKENED_EXPRESSION
	 * 		in the case of commas there needs to be a way to determine how much of the tokens stack belongs on either side of the comma
	 * @param	part	- the expression to detoken - this is either the left or right of a comma in the tokens.expression value
	 * @param	tokens	- the tokens container that part was derived from
	 * @return	a detokened representation of part
	 */
	private static function detokenize(part:String, tokens:Tokens_t):String
	{	//force a clone to perform surgery on
		var expr:String = part.substring(0);								
		
		regex.token.map(part, function(reg:EReg):String
		{	//the current matched token
			var result	= reg.matched(0);									
			
			//derive the stack index from the token number
			regex.toknum.match(result);								
			var stk_i:Int = Std.parseInt(regex.toknum.matched(0));	
			
			//replace the token with the expression it refers to in the stack
			expr = StringTools.replace(expr, result, tokens.stack[stk_i]);	
			
			//satisfy regex.map() requirement, no change is made
			return result;
		});
		
		//the decrypted expr
		return expr;
	}
	
	/** PARSE_THE_END_RESULT
	 * @param	expr	- the expression to parse
	 * @param	value	- if the expression is a function or group this will contain its inner string value
	 * @return	the float value of the supplied string expression
	 */
	private static function parseType(expr:String, ?value:String):Float
	{
		//becomes the result of various regex matches
		var result:String;
		var final:Float = 0.0; 
		
		//helper vars
		var csv:Array<String>;
		var a:Float, b:Float;
		
		//to clone math functions
		var func1:Float->Float;
		var func2:Float->Float->Float;
		
		var neg:Bool = (expr.charAt(0) == "-");
		
		//if this expression has inner value - ie...it's a func or group
		if (value != null)
		{	
			//if this is a math function
			if (regex.funcs.match(expr))
			{	//grab result
				result = regex.funcs.matched(0);
				
				//tokenize the inner expression so it can be properly split on comma
				var tokens:Tokens_t = tokenize(value);
				
				//split on comma
				csv = (new EReg(",", "g")).split(tokens.expression);
				
				//no math function expects more than 2 arguments
				if (csv.length > 2) return Math.NaN;
				
				//if 2 arguments exist - process as a 2 arguments function
				if (csv.length == 2)
				{	
					//process the arguments
					a = parse(detokenize(csv[0], tokens));
					b = parse(detokenize(csv[1], tokens));
					
					//clone the necessary function and apply it to the args
					func2 = Reflect.getProperty(Math, result.toLowerCase());
					final = Reflect.callMethod(Math, func2, [a, b]);
					
				} else {	/* process as a single argument function */
					
					//random has no args so, just catch it and call it
					if (result == "random")
						final = Math.random();
					else 
					{	//process the arguments
						a = parse(value);
						
						//clone the necessary function and apply it to the argument
						func1 = Reflect.getProperty(Math, result.toLowerCase());
						final = Reflect.callMethod(Math, func1, [a]);
					}
				}
			} else final = parse(value);			//group expression
			
		} else if (regex.constant.match(expr)) {	//check if constant
			
			result = regex.constant.matched(0);
			final = Reflect.getProperty(Math, result.toUpperCase());
			
		} else {
			if (expr == "random")
				final = Math.random();
			else final = Std.parseFloat(expr);		//must be a number
		}
	
		//apply negative
		if(final > -1)
			final = (neg)? -final : final;
			
		return final;
	}
	
	/** GET_OPERATION_VALUE
	 * @param	val1 - the first value
	 * @param	oper - operator
	 * @param	val2 - second value
	 * @return	the result or NaN
	 */
	private static function value(val1:Float, oper:String, val2:Float):Float
	{
		//switch over possible operators and apply the appropriate operation
		switch(oper)
		{	case "*": return (val1 * val2);
			case "/": return (val1 / val2);
			case "+": return (val1 + val2);
			case "-": return (val1 - val2);
		}
		
		//unknown operator returns NaN
		return Math.NaN;
	}
}
