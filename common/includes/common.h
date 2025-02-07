
#ifndef _COMMON_H_
#define _COMMON_H_

// Disable the warning C4706 -> assignment within conditional expression
// Allow us to use Warning level 4 instead of level 3
#pragma warning( disable : 4706)
#pragma warning( disable : 4786)	// Debug symbols thing
#pragma warning( disable : 4996)   // #define _CRT_SECURE_NO_WARNINGS
//#define _CRT_SECURE_NO_WARNINGS


#include <string>
#include <vector>


//
// Debugging macros
//
#ifdef  _DEBUG
#define _BREAK_					__asm { int 3 }
#define _BREAK_IF_(cond)		if (cond) __asm { int 3 } else ((void)0)
#else
#define _BREAK_					((void)0)
#define _BREAK_IF_(cond)		((void)0)
#endif


//
// Argument parsing, error handling
//
void SetApplicationParameters(const char* pcApplicationName,int nVersionMajor,int nVersionMinor,const char* pcUsageMessage);

void ShowError(const char *pcMessage);


class ArgumentParser
{
public:
	ArgumentParser(int argc,char *argv[]);
	~ArgumentParser() {}

	const char* GetParameter(int nParameterIndex);
	int GetParameterCount();

	bool ProcessNextArgument();

	bool IsSwitch(const char *ptr_switch);
	bool IsParameter();

	std::string GetStringValue();
	int GetIntegerValue(int default_value);
	bool GetBooleanValue(bool default_value);
	bool GetSeparator(const char* ptr_separator_list);
	const char* GetRemainingStuff();

private:
	ArgumentParser();

private:
	int			m_argc;
	char**		m_argv;
    long		m_first_param;

	int			m_remaining_argc;
	long		m_nb_arg;
	const char *m_ptr_arg;
};

bool get_switch(const char *&ptr_arg,const char *ptr_switch);
int get_value(const char *&ptr_arg,long default_value);
std::string get_string(const char *&ptr_arg);


//
// File loading and saving
//
bool LoadFile(const char* pcFileName,void* &pcBuffer,size_t &cBufferSize);
bool SaveFile(const char* pcFileName,const void* pcBuffer,size_t cBufferSize);
bool DeleteFile(const char* pcFileName);

bool LoadText(const char* pcFileName,std::vector<std::string>& cTextData);


//
// Preprocessing and filtering
//
int StringReplace(std::string& cMainString,const std::string& cSearchedString,const std::string& cReplaceString);
std::string StringTrim(const std::string& cInputString,const std::string& cFilteredOutCharacterList=" \t\f\v\n\r");
std::string StringFormat(const char* pFormatString,...);

int ConvertAdress(const char *ptr_value);

class TextFileGenerator
{
public:
	enum Language_e
	{
		_eLanguage_Undefined_,
		eLanguage_C,
		eLanguage_Assembler,
		eLanguage_BASIC
	};

	enum Endianness_e
	{
		_eEndianness_Undefined_,
		_eEndianness_Little,
		_eEndianness_Big
	};

	enum NumericBase_e
	{
		_eNumericBase_Undefined_,
		_eNumericBase_Hexadecimal,
		_eNumericBase_Decimal
	};

public:
	TextFileGenerator();
	~TextFileGenerator();

	void SetDataSize(int nDataSize)								{ m_nDataSize=nDataSize; }
	void SetFileType(Language_e nFileType)						{ m_nFileType=nFileType; }
	void SetEndianness(Endianness_e nEndianness)				{ m_nEndianness=nEndianness; }
	void SetNumericBase(NumericBase_e nNumericBase)				{ m_nNumericBase=nNumericBase; }
	void SetValuesPerLine(unsigned int nValuesPerLine)			{ m_nValuesPerLine=nValuesPerLine; }
	void SetLabel(const std::string& cLabel)					{ m_cLabelName=cLabel; }
	void SetLineNumber(int nLineNumber)							{ m_nFirstLineNumber=nLineNumber; }
	void SetIncrementLineNumber(int nIncrementLineNumber)		{ m_nIncrementLineNumber=nIncrementLineNumber; }

	int GetDataSize()				{ return m_nDataSize; }
	Language_e GetFileType()		{ return m_nFileType; }
	Endianness_e GetEndianness()	{ return m_nEndianness; }
	NumericBase_e GetNumericBase()	{ return m_nNumericBase; }
	unsigned int GetValuesPerLine()	{ return m_nValuesPerLine; }
	const std::string& GetLabel()	{ return m_cLabelName; }
	int GetLineNumber()				{ return m_nFirstLineNumber; }
	int GetIncrementLineNumber()	{ return m_nIncrementLineNumber; }

	void ConvertData(std::string& cDestString,const void* pSourceData,size_t nFileSize);

private:
	int				m_nDataSize;
	Language_e		m_nFileType;
	Endianness_e	m_nEndianness;
	NumericBase_e	m_nNumericBase;
	unsigned int	m_nValuesPerLine;

	bool			m_bEnableLineNumber;
	int				m_nFirstLineNumber;
	int				m_nIncrementLineNumber;

	std::string		m_cLabelName;
};

#ifndef __WINDOWS__

#define getch getchar
#include <sys/types.h>

#define _open open
#define _close close
#define _read read
#define _write write
#define _unlink unlink

#define O_BINARY 0
#define _O_TRUNC O_TRUNC
#define _O_CREAT O_CREAT
#define _S_IREAD S_IREAD
#define _S_IWRITE S_IWRITE	

#define _MAX_PATH 512

#define stricmp strcasecmp
#define memicmp memcmp

#endif

#endif // _COMMON_H_
