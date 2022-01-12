
class Lock 
{
      public:
            Lock(CRITICAL_SECTION& cs) : cs(cs) { EnterCriticalSection(&cs); };
            ~Lock() {LeaveCriticalSection(&cs); };
      private:
            CRITICAL_SECTION& cs;
};
