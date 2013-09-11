using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace pygmalion
{
    class Token
    {
        public TokenType type, assignOp = TokenType.NULL;
        public object value;
        public int lineno, start, end;
        public Token() { this.type = TokenType.NULL; }
        public Token(TokenType type) { this.type = type; }
		
		public static bool IsEqual(Token t1, Token t2)
		{
			if (t1==t2)
				return true;
			if (t1==null||t2==null)
				return false;
			if (t1.value!=t2.value)
			{
				if (t1.value==null||t2.value==null)
					return false;
				if (t1.value.ToString()!=t2.value.ToString())
					return false;
			}
			return t1.type==t2.type&&
				t1.assignOp==t2.assignOp&&
				// ignored - old tokenizer has bug: t1.lineno==t2.lineno&&
				t1.start==t2.start&&
				t1.end==t2.end;
		}
		
		public override string ToString() 
		{
			return ""+type+"|"+assignOp+"|"+value+"|"+lineno+"|"+start+"|"+end;
		}
    }
}
