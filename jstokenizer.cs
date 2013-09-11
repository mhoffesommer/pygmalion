// if running in editor then perform tokenizer validation by running
// the old one and the new one in parallel and comparing the results
// (disabled by default since old tokenizer is really slow...)
#if UNITY_EDITOR
//#	define	JS_TOKENIZER_TEST
#endif

using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;

namespace pygmalion
{
#if JS_TOKENIZER_TEST
	class NewTokenizer : JSObject
#else
    class Tokenizer : JSObject
#endif
    {
		readonly string		m_source;
		readonly string		m_fileName;
		int					m_lineNr;
		int					m_cursor=0;
		bool				m_scanNewLines=false;
		bool				m_scanOperand=true;
		Token[]				m_tokens=new Token[4]; // circular buffer
		int					m_tokenIndex=0;
		int					m_lookAhead=0;
		
        public 
#if JS_TOKENIZER_TEST
			NewTokenizer
#else
			Tokenizer
#endif
				(ExecutionContext GLOBAL, string src, string filename=null, int lineNr=1)
        {
			for (int k=0;k<m_tokens.Length;++k)
				m_tokens[k]=new Token();
			m_source=src;
			m_fileName=filename!=null?filename:"";
			m_lineNr=filename!=null?lineNr:1;
		}
		
		public string source
		{
			get { return m_source; }
		}
		
		public bool scanOperand
		{
			get { return m_scanOperand; } 
			set { m_scanOperand=value; }
		}
		
		public int lineno
		{
			get { return m_lineNr; }
		}
		
		public string filename
		{
			get { return m_fileName; }
		}
		
        public string input
        {
            get { return m_source.Substring(m_cursor); }
        }

        public bool done
        {
            get { return peek()==TokenType.END; }
        }
		
        public Token token
        {
            get { return m_tokens[m_tokenIndex]; }
		}
		
        [GlobalArg]
        public bool match(TokenType tt)
        {
			if (get()==tt)
				return true;
			unget();
			return false;
        }

        public Token mustMatch(TokenType tt)
        {
            if (!this.match(tt))
                throw this.newSyntaxError("Missing " + jsdefs.tokens[tt].ToLower());
            return token;
        }
		
        public TokenType peek()
        {
            TokenType tt;
            if (m_lookAhead != 0)
            {
                Token next = m_tokens[(m_tokenIndex + m_lookAhead) % m_tokens.Length];
                if (m_scanNewLines && next.lineno != m_lineNr)
                    tt = TokenType.NEWLINE;
                else
                    tt = next.type;
            }
            else
            {
                tt = get();
                unget();
            }
            return tt;
		}
		
        public TokenType peekOnSameLine()
        {
            m_scanNewLines = true;
            TokenType tt = peek();
            m_scanNewLines = false;
            return tt;
        }
		
        public TokenType get()
		{
			Token t;
			while (m_lookAhead!=0)
			{
				--m_lookAhead;
				m_tokenIndex=(m_tokenIndex+1)%m_tokens.Length;
				t=m_tokens[m_tokenIndex];
				if (t.type!=TokenType.NEWLINE||m_scanNewLines)
					return t.type;
			}
			
			// skip over whitespace, comments
			char cur,next;
			while (m_cursor<m_source.Length)
			{
				cur=m_source[m_cursor];
				
				// space, tab
				if (cur==' '||cur=='\t')
				{
					++m_cursor;
					continue;
				}
				
				// all whitespace (unless m_scanNewLines on)
				if (!m_scanNewLines)
				{
					if (cur<' ') // whitespace effectively...
					{
						if (cur=='\n')
							++m_lineNr;
						++m_cursor;
						continue;
					}
				}
				
				// comments?
				if (cur!='/'||m_cursor+1==m_source.Length)
					break;
				next=m_source[m_cursor+1];
				if (next=='/')
				{
					// single line comment
					while (++m_cursor<m_source.Length)
						if (m_source[m_cursor]=='\n')
							break;
				}
				else if (next=='*')
				{
					// multi line comment
					while (++m_cursor<m_source.Length-1)
					{
						cur=m_source[m_cursor];
						if (cur=='\n')
						{
							++m_lineNr;
							continue;
						}
						if (cur=='*'&&m_source[m_cursor+1]=='/')
						{
							m_cursor+=2;
							break;
						}
					}
				}
				else
				{
					// not a comment, done
					break;
				}
			}
			
			m_tokenIndex=(m_tokenIndex+1)%m_tokens.Length;
			t=m_tokens[m_tokenIndex];
			t.start=m_cursor;
			
			if (m_cursor==m_source.Length)
			{
				t.type=TokenType.END;
				return TokenType.END;
			}
			
			cur=m_source[m_cursor];
			next=m_cursor+1<m_source.Length?m_source[m_cursor+1]:' ';
			if ((cur>='0'&&cur<='9')||(cur=='.'&&next>='0'&&next<='9'))
			{
				t.type=TokenType.NUMBER;
				
				if (++m_cursor<m_source.Length)
				{
					if (next=='x'||next=='X')
					{
						while (++m_cursor<m_source.Length)
						{
							next=m_source[m_cursor];
							if (next<'0'||(next>'9'&&next<'A')||(next>'F'&&next<'a')||next>'f')
								break;
						}
						t.value=(double)Convert.ToInt64(m_source.Substring(t.start+2,m_cursor-(t.start+2)),16);
					}
					else if (cur=='0'&&next>='0'&&next<'7')
					{
						while (++m_cursor<m_source.Length)
						{
							next=m_source[m_cursor];
							if (next<'0'||next>'7')
								break;
						}
						t.value=(double)Convert.ToInt64(m_source.Substring(t.start,m_cursor-t.start),8);
					}
					else
					{
						--m_cursor;
						while (++m_cursor<m_source.Length)
						{
							next=m_source[m_cursor];
							if (next>='0'&&next<='9')
								continue;
							if (next=='.')
								continue;
							if (next=='e'||next=='E')
							{
								next=m_source[m_cursor];
								if (next=='+'||next=='-')
									++m_cursor;
								continue;
							}

							break;
						}
						
						t.value=double.Parse(m_source.Substring(t.start,m_cursor-t.start));
					}
				}
			}
			else if (cur=='$'||cur=='_'||(cur>='a'&&cur<='z')||(cur>='A'&&cur<='Z'))
			{
				while (++m_cursor<m_source.Length)
				{
					next=m_source[m_cursor];
					if (next=='$'||next=='_')
						continue;
					if (next>='a'&&next<='z')
						continue;
					if (next>='A'&&next<='Z')
						continue;
					if (next>='0'&&next<='9')
						continue;
					break;
				}
				
				string s=m_source.Substring(t.start,m_cursor-t.start);
				t.value=s;
				if (!jsdefs.keywords.TryGetValue(s,out t.type))
					t.type=TokenType.IDENTIFIER;
			}
			else if (cur=='\''||cur=='"')
			{
				t.type=TokenType.STRING;
				int matched;
				t.value=JSObject.StringLiteral(m_source.Substring(t.start),out matched);
				m_cursor+=matched+2;
			}
			else
			{
				if (m_scanOperand&&cur=='/')
				{
			        // A regexp to match regexp literals.
			        Regex reRegExp = new Regex("^\\/((?:\\.|\\[(?:\\.|[^\\]])*\\]|[^\\/])+)\\/([gimy]*)");
			        
					Match capMatch;
		            if (this.scanOperand && (capMatch = reRegExp.Match(m_source.Substring(m_cursor))).Success)
		            {
		                t.type = TokenType.REGEXP;
		                t.value = new Regex(capMatch.Groups[1].Value);
						m_cursor += capMatch.Groups [0].Value.Length;
						t.end=m_cursor;
						t.lineno=m_lineNr;
						return t.type;
		            }
				}
				
				t.assignOp=TokenType.NULL;
				++m_cursor;
				
				switch(cur)
				{
				case '\n':	t.type=TokenType.NEWLINE; break;
				case ';':	t.type=TokenType.SEMICOLON; break;
				case ',':	t.type=TokenType.COMMA; break;
				case '?':	t.type=TokenType.HOOK; break;
				case ':':	t.type=TokenType.COLON; break;
				case '~':	t.type=TokenType.BITWISE_NOT; break;
				case '.':	t.type=TokenType.DOT; break;
				case '[':	t.type=TokenType.LEFT_BRACKET; break;
				case ']':	t.type=TokenType.RIGHT_BRACKET; break;
				case '{':	t.type=TokenType.LEFT_CURLY; break;
				case '}':	t.type=TokenType.RIGHT_CURLY; break;
				case '(':	t.type=TokenType.LEFT_PAREN; break;
				case ')':	t.type=TokenType.RIGHT_PAREN; break;
				case '*': 	t.type=TokenType.MUL; break;
				case '/':	t.type=TokenType.DIV; break;
				case '%':	t.type=TokenType.MOD; break;
				case '^':	t.type=TokenType.BITWISE_XOR; break;
					
				case '|':
					if (m_cursor<m_source.Length&&m_source[m_cursor]=='|')
					{
						t.type=TokenType.OR;
						++m_cursor;
					}
					else
						t.type=TokenType.BITWISE_OR;
					break;
				case '&':
					if (m_cursor<m_source.Length&&m_source[m_cursor]=='&')
					{
						t.type=TokenType.AND;
						++m_cursor;
					}
					else
						t.type=TokenType.BITWISE_AND;
					break;
				case '=':
					if (m_source.Substring(m_cursor,2)=="==")
					{
						t.type=TokenType.STRICT_EQ;
						m_cursor+=2;
					}
					else if (m_source.Substring(m_cursor,1)=="=")
					{
						t.type=TokenType.EQ;
						m_cursor+=1;
					}
					else
						t.type=TokenType.ASSIGN;
					break;
				case '!':
					if (m_source.Substring(m_cursor,2)=="==")
					{
						t.type=TokenType.STRICT_NE;
						m_cursor+=2;
					}
					else if (m_source.Substring(m_cursor,1)=="=")
					{
						t.type=TokenType.NE;
						m_cursor+=1;
					}
					else
						t.type=TokenType.NOT;
					break;
				case '+':
					if (m_cursor<m_source.Length&&m_source[m_cursor]=='+')
					{
						t.type=TokenType.INCREMENT;
						++m_cursor;
					}
					else
						t.type=TokenType.PLUS;
					break;
				case '-':
					if (m_cursor<m_source.Length&&m_source[m_cursor]=='-')
					{
						t.type=TokenType.DECREMENT;
						++m_cursor;
					}
					else
						t.type=TokenType.MINUS;
					break;
				case '<':
					if (m_source.Substring(m_cursor,1)=="<")
					{
						t.type=TokenType.LSH;
						m_cursor+=1;
					}
					else if (m_source.Substring(m_cursor,1)=="=")
					{
						t.type=TokenType.LE;
						m_cursor+=1;
					}
					else
						t.type=TokenType.LT;
					break;
				case '>':
					if (m_source.Substring(m_cursor,2)==">>")
					{
						t.type=TokenType.URSH;
						m_cursor+=2;
					}
					else if (m_source.Substring(m_cursor,1)==">")
					{
						t.type=TokenType.RSH;
						m_cursor+=1;
					}
					else if (m_source.Substring(m_cursor,1)=="=")
					{
						t.type=TokenType.GE;
						m_cursor+=1;
					}
					else
						t.type=TokenType.GT;
					break;
					
				default:
					throw newSyntaxError("Illegal token");
				}
				
				t.value=m_source.Substring(t.start,m_cursor-t.start);
				
				if (m_cursor<m_source.Length&&m_source[m_cursor]=='=')
				{
					switch(t.type)
					{
					case TokenType.BITWISE_OR:
					case TokenType.BITWISE_XOR:
					case TokenType.BITWISE_AND:
					case TokenType.LSH:
					case TokenType.RSH:
					case TokenType.URSH:
					case TokenType.PLUS:
					case TokenType.MINUS:
					case TokenType.MUL:
					case TokenType.DIV:
					case TokenType.MOD:
						t.assignOp=t.type;
						t.type=TokenType.ASSIGN;
						++m_cursor;
						break;
					}
				}
				
				if (m_scanOperand)
				{
					switch(t.type)
					{
					case TokenType.PLUS:	t.type=TokenType.UNARY_PLUS; break;
					case TokenType.MINUS:	t.type=TokenType.UNARY_MINUS; break;
					}
				}
			}

			t.end=m_cursor;
			t.lineno=m_lineNr;
			return t.type;
		}
		
        public void unget()
        {
            if (++m_lookAhead==m_tokens.Length) 
				throw new Exception("PANIC: too much lookahead!");
            m_tokenIndex=(m_tokenIndex+m_tokens.Length-1)%m_tokens.Length;
		}
		
        public Exception newSyntaxError(string m)
        {
            SyntaxError e = new SyntaxError(m, filename, lineno);
            e.source = m_source;
            e.cursor = m_cursor;
            return e;
		}
	}
	
#if UNITY_EDITOR
	// this is the old (horribly) slow tokenizer; it's kept around in editor-only builds as a baseline comparison
	// against the new faster tokenizer; output from Tokenizer and LegacyTokenizer should be the same
	class LegacyTokenizer: JSObject
	{
        int cursor;
        public string source;
        public Dictionary<int, Token> tokens = new Dictionary<int, Token>();
        public int tokenIndex, lookahead;
        public bool scanNewlines, scanOperand;
        public string filename;
        public int lineno;
        public ExecutionContext GLOBAL;
        // Build a regexp that recognizes operators and punctuators (except newline).
        public static Regex opRegExp;
        // A regexp to match floating point literals (but not integer literals).
        public static Regex fpRegExp = new Regex("^\\d+\\.\\d*(?:[eE][-+]?\\d+)?|^\\d+(?:\\.\\d*)?[eE][-+]?\\d+|^\\.\\d+(?:[eE][-+]?\\d+)?");
        // A regexp to match regexp literals.
        public static Regex reRegExp = new Regex("^\\/((?:\\.|\\[(?:\\.|[^\\]])*\\]|[^\\/])+)\\/([gimy]*)");
        // A regexp to match eol
        public static Regex elRegExp = new Regex("\n");
        // A regexp for hex numbers
        public static Regex hxRegExp = new Regex("^0[xX][\\da-fA-F]+|^0[0-7]*|^\\d+");
        // A regexp for regex escapes
        public static Regex ecRegExp = new Regex("[?|^&(){}\\[\\]+\\-*\\/\\.]");
        // A regexp for scan newlines mode
        public static Regex snRegExp = new Regex("^[ \t]+");
        // A regexp for no newlines mode
        public static Regex nnRegExp = new Regex("^\\s+");
        // A comments regex
        public static Regex cmRegExp = new Regex("^\\/(?:\\*(?:.|\n)*?\\*\\/|\\/.*)");
        // An identifier regex
        public static Regex idRegExp = new Regex("^[$_\\w]+");
        // A quotes regex
        public static Regex qtRegExp = new Regex("^\"|^'");
        // A char matching regex
        public static Regex dtRegExp = new Regex(".*");

        internal static void InitTokenizer()
        {
            string opRegExpSrc = "^";
            foreach (string i in jsdefs.opTypeNames.Keys)
            {
                if (i == "\n")
                    continue;
                if (opRegExpSrc != "^")
                    opRegExpSrc += "|^";
                opRegExpSrc += ecRegExp.Replace(i, "\\$&");
            }
            opRegExp = new Regex(opRegExpSrc);
        }

        public LegacyTokenizer(ExecutionContext GLOBAL, string s, string f, int l)
        {
            this.GLOBAL = GLOBAL;
            this.cursor = 0;
            this.source = s;
            this.tokenIndex = 0;
            this.lookahead = 0;
            this.scanNewlines = false;
            this.scanOperand = true;
            this.filename = f != null ? f : "";
            this.lineno = f != null ? l : 1;
        }

        public string input
        {
            get
            {
                return this.source.Substring(this.cursor);
            }
        }

        public bool done
        {
            get
            {
                return this.peek() == TokenType.END;
            }
        }

        public Token token
        {
            get
            {
                Token result;
                if (!this.tokens.TryGetValue(this.tokenIndex, out result))
                    return null;
                else
                    return result;
            }
        }

        [GlobalArg]
        public bool match(TokenType tt)
        {
            return this.get() == tt || JSObject.ToBool(GLOBAL, this.unget());
        }

        public Token mustMatch(TokenType tt)
        {
            if (!this.match(tt))
                throw this.newSyntaxError("Missing " + jsdefs.tokens[tt].ToLower());
            return this.token;
        }

        public TokenType peek()
        {
            TokenType tt;
            Token next;
            if (this.lookahead != 0)
            {
                next = this.tokens[(this.tokenIndex + this.lookahead) & 3];
                if (this.scanNewlines && next.lineno != this.lineno)
                    tt = TokenType.NEWLINE;
                else
                    tt = next.type;
            }
            else
            {
                tt = this.get();
                this.unget();
            }
            return tt;
        }

        public TokenType peekOnSameLine()
        {
            this.scanNewlines = true;
            TokenType tt = this.peek();
            this.scanNewlines = false;
            return tt;
        }

        public TokenType get()
        {
            Token token;
            while (this.lookahead != 0)
            {
                --this.lookahead;
                this.tokenIndex = (this.tokenIndex + 1) & 3;
                token = this.tokens[this.tokenIndex];
                if (token.type != TokenType.NEWLINE || this.scanNewlines)
                    return token.type;
            }

            string capInput;
            Match capMatch;

            for (;;)
            {
                capInput = this.input;
                capMatch = (this.scanNewlines ? snRegExp : nnRegExp).Match(capInput);
                if (capMatch.Success)
                {
                    string spaces = capMatch.Value;
                    this.cursor += spaces.Length;
                    Match nlMatch = elRegExp.Match(spaces);
                    if (nlMatch.Value != "")
                        this.lineno += nlMatch.Length;
                    capInput = this.input;
                }

                if (!(capMatch = cmRegExp.Match(capInput)).Success)
                    break;
                string comment = capMatch.Value;
                this.cursor += comment.Length;
                string newlines = elRegExp.Match(comment).Value;
                if (newlines != "")
                    this.lineno += newlines.Length;
            }

            this.tokenIndex = (this.tokenIndex + 1) & 3;
            if (!this.tokens.TryGetValue(this.tokenIndex, out token))
                token = null;

            if (token == null)
            {
                token = new Token();
                this.tokens[this.tokenIndex] = token;
            }

            if (capInput == "")
                return token.type = TokenType.END;

            if ((capMatch = fpRegExp.Match(capInput)).Success)
            {
                token.type = TokenType.NUMBER;
                token.value = JSObject.ToNumber(GLOBAL, capMatch.Groups[0].Value);
            }
            else if ((capMatch = hxRegExp.Match(capInput)).Success)
            {
                token.type = TokenType.NUMBER;
                token.value = JSObject.ToNumber(GLOBAL, capMatch.Groups[0].Value);
            }
            else if ((capMatch = idRegExp.Match(capInput)).Success) // FIXME no ES3 unicode
            {       
                string id = capMatch.Groups[0].Value;
                if (!jsdefs.keywords.TryGetValue(id, out token.type))
                    token.type = TokenType.IDENTIFIER;
                token.value = id;
            }
            else if ((capMatch = qtRegExp.Match(capInput)).Success) //"){
            {
                char matchCh = capMatch.Groups[0].Value[0];
                int matched;
                token.type = TokenType.STRING;
                token.value = JSObject.StringLiteral(capInput, out matched);
                capMatch = new Regex("^" + matchCh + ".{" + matched.ToString() + "}" + matchCh).Match(capInput);
            }
            else if (this.scanOperand && (capMatch = reRegExp.Match(capInput)).Success)
            {
                token.type = TokenType.REGEXP;
                token.value = new Regex(capMatch.Groups[1].Value); //, capMatch.Groups[2].Value);
            }
            else if ((capMatch = opRegExp.Match(capInput)).Success)
            {
                string op = capMatch.Groups[0].Value;
                if (jsdefs.assignOps.ContainsKey(op) && capInput[op.Length] == '=')
                {
                    token.type = TokenType.ASSIGN;
                    token.assignOp = jsdefs.assignOps[op];
                    capMatch = dtRegExp.Match(op + "=");
                }
                else
                {
                    token.type = jsdefs.tokenWords[op];
                    if (this.scanOperand &&
                       (token.type == TokenType.PLUS))
                        token.type = TokenType.UNARY_PLUS;
                    if (this.scanOperand &&
                        (token.type == TokenType.MINUS))
                        token.type = TokenType.UNARY_MINUS;
                    token.assignOp = TokenType.NULL;
                }
                token.value = op;
            }
            else if (this.scanNewlines && (capMatch = (elRegExp.Match(capInput))).Success)
            {
                token.type = TokenType.NEWLINE;
            }
            else
            {
                throw this.newSyntaxError("Illegal token");
            }

            token.start = this.cursor;
            this.cursor += capMatch.Groups[0].Value.Length;
            token.end = this.cursor;
            token.lineno = this.lineno;
            return token.type;
        }

        public object unget()
        {
            if (++this.lookahead == 4) throw new Exception("PANIC: too much lookahead!");
            this.tokenIndex = (this.tokenIndex - 1) & 3;
            return JSUndefined.Undefined;
        }

        public Exception newSyntaxError(string m)
        {
            SyntaxError e = new SyntaxError(m, this.filename, this.lineno);
            e.source = this.source;
            e.cursor = this.cursor;
            return e;
        }
    }
#endif
	
#if JS_TOKENIZER_TEST
	// 'Tokenizer' that wraps both legacy and new one and compares the results of each
	class Tokenizer: JSObject
	{
		NewTokenizer	m_new;
		LegacyTokenizer	m_old;
			
        public Tokenizer(ExecutionContext GLOBAL, string src, string filename, int lineNr)
		{
			m_new=new NewTokenizer(GLOBAL,src,filename,lineNr);
			m_old=new LegacyTokenizer(GLOBAL,src,filename,lineNr);
		}
		
		void Check(bool cond, string msg="")
		{
			if (!cond)
			{
				UnityEngine.Debug.LogError("Tokenizer validation failed: "+msg);
				throw newSyntaxError("Validation failed");
			}
		}
		
		public string source
		{
			get { return m_new.source; }
		}
		
		public bool scanOperand
		{
			get { return m_new.scanOperand; } 
			set { m_new.scanOperand=value; m_old.scanOperand=value; }
		}
		
		public int lineno
		{
			get 
			{ 
				// ignored, old tokenizer has bug: Check(m_old.lineno==m_new.lineno);
				return m_new.lineno; 
			}
		}
		
		public string filename
		{
			get { return m_new.filename; }
		}
		
        public string input
        {
            get { return m_new.input; }
        }

        public bool done
        {
            get 
			{ 
				Check (m_old.done==m_new.done);
				return m_new.done; 
			}
        }
		
        public Token token
        {
            get 
			{
				Token t1=m_old.token,t2=m_new.token;
				if (t1==null&&t2.type==TokenType.NULL)
					return t2;
				Check (Token.IsEqual(t1,t2),"Old:"+t1+"/New:"+t2);
				return t2;
			}
		}
		
        public bool match(TokenType tt)
        {
			if (get()==tt)
				return true;
			unget();
			return false;
        }

        public Token mustMatch(TokenType tt)
        {
            if (!this.match(tt))
                throw this.newSyntaxError("Missing " + jsdefs.tokens[tt].ToLower());
            return token;
        }
		
        public TokenType peek()
        {
			Check (m_old.peek()==m_new.peek());
			return m_new.peek ();
		}
		
        public TokenType peekOnSameLine()
        {
			Check (m_old.peekOnSameLine()==m_new.peekOnSameLine());
			return m_new.peekOnSameLine();
        }
		
        public TokenType get()
		{
			TokenType t1=m_old.get (),t2=m_new.get ();
			Check (t1==t2);
			return t2;
        }

        public void unget()
        {
			m_old.unget ();
			m_new.unget ();
        }

        public Exception newSyntaxError(string m)
        {
			return m_new.newSyntaxError(m);
        }
	}
#endif
	
    public class SyntaxError : Exception
    {
        string mFilename;
        public string Filename { get { return mFilename; } }
        int mLine;
        public int Line { get { return mLine; } }
        public string source;
        public int cursor;
        public SyntaxError(string m, string f, int l)
            : base(m)
        {
            mFilename = f;
            mLine = l;
        }
    }
}
