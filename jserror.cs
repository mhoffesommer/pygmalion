namespace pygmalion
{
	class JSError : JSObject
	{
		string	m_msg,m_fn;
		int		m_line;

		public override string Class { get { return "Error"; } }

		public JSError(ExecutionContext GLOBAL): this(GLOBAL,"","",0)
		{
		}

		public JSError(ExecutionContext GLOBAL, string msg): this(GLOBAL,msg,"",0)
		{
		}

		public JSError(ExecutionContext GLOBAL, string msg, string fn): this(GLOBAL,msg,fn,0)
		{
		}

		public JSError(ExecutionContext GLOBAL, string msg, string fn, double ln)
		{
			m_msg = msg;
			m_fn = fn;
			m_line = (int)ln;
		}

		public override string ToString()
		{
			return string.Format ("ERROR {0} in {1}({2})", m_msg, m_fn, m_line);
		}
	}
}
