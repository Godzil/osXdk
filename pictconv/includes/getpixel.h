
#ifndef GET_PIXEL_H
#define GET_PIXEL_H


class RgbColor
{
public:
	RgbColor()						{ m_red=0;m_green=0;m_blue=0;}
	RgbColor(const RgbColor& rgb)	{ m_red=rgb.m_red;m_green=rgb.m_green;m_blue=rgb.m_blue;}
	RgbColor(unsigned char red,unsigned char green,unsigned char blue) { m_red=red;m_green=green;m_blue=blue;}

	bool operator< ( const RgbColor& rhs ) const { return (m_red<rhs.m_red) && (m_green<rhs.m_green) && (m_blue<rhs.m_blue); }

	bool operator== ( const RgbColor& rhs ) const { return (m_red==rhs.m_red) && (m_green==rhs.m_green) &&  (m_blue==rhs.m_blue) ; }
	bool operator!= ( const RgbColor& rhs ) const { return !(*this==rhs) ; }

public:
	unsigned char m_red;
	unsigned char m_green;
	unsigned char m_blue;
};


#endif