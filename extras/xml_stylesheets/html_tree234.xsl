<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<!-- 
	This template produces an HTML document for 234 trees.
-->
<xsl:strip-space elements="*" />
<xsl:template match="/" priority="200">
	<html>
	<head>
	<title>234 Tree</title>
	</head>
	<body>
	<table border="1">
	<tr>
		<td>Key</td><td>Path</td><td>Value</td><td>Path</td>
	</tr>
	<xsl:apply-templates />
	</table>
	</body>
	</html>
</xsl:template>

<xsl:template match="empty" priority="50">
</xsl:template>

<xsl:template match="two" priority="50">
	<xsl:param name="termpath" />
	<xsl:apply-templates select="*[position() = 3]" >
		<xsl:with-param name="termpath">
			<xsl:value-of select="concat($termpath, '/', 3)"/>
		</xsl:with-param>
	</xsl:apply-templates>
	<tr>
	<td><xsl:apply-templates select="*[position()=1]"/></td>
	<td><xsl:value-of select="concat($termpath, '/', 1)"/></td>
	<td><xsl:apply-templates select="*[position()=2]"/></td>
	<td><xsl:value-of select="concat($termpath, '/', 2)"/></td>
	</tr>
	<xsl:apply-templates select="*[position() = 4]" >
		<xsl:with-param name="termpath">
			<xsl:value-of select="concat($termpath, '/', 4)"/>
		</xsl:with-param>
	</xsl:apply-templates>
</xsl:template>
<xsl:template match="three" priority="50">
	<xsl:param name="termpath" />
	<xsl:apply-templates select="*[position() = 5]" >
		<xsl:with-param name="termpath">
			<xsl:value-of select="concat($termpath, '/', 5)"/>
		</xsl:with-param>
	</xsl:apply-templates>
	<tr>
	<td><xsl:apply-templates select="*[position()=1]"/></td>
	<td><xsl:value-of select="concat($termpath, '/', 1)"/></td>
	<td><xsl:apply-templates select="*[position()=2]"/></td>
	<td><xsl:value-of select="concat($termpath, '/', 2)"/></td>
	</tr>
	<xsl:apply-templates select="*[position() = 6]" >
		<xsl:with-param name="termpath">
			<xsl:value-of select="concat($termpath, '/', 6)"/>
		</xsl:with-param>
	</xsl:apply-templates>
	<tr>
	<td><xsl:apply-templates select="*[position()=3]"/></td>
	<td><xsl:value-of select="concat($termpath, '/', 3)"/></td>
	<td><xsl:apply-templates select="*[position()=4]"/></td>
	<td><xsl:value-of select="concat($termpath, '/', 4)"/></td>
	</tr>
	<xsl:apply-templates select="*[position() = 7]" >
		<xsl:with-param name="termpath">
			<xsl:value-of select="concat($termpath, '/', 7)"/>
		</xsl:with-param>
	</xsl:apply-templates>
</xsl:template>
<xsl:template match="four" priority="50">
	<xsl:param name="termpath" />
	<xsl:apply-templates select="*[position() = 7]" >
		<xsl:with-param name="termpath">
			<xsl:value-of select="concat($termpath, '/', 7)"/>
		</xsl:with-param>
	</xsl:apply-templates>
	<tr>
	<td><xsl:apply-templates select="*[position()=1]"/></td>
	<td><xsl:value-of select="concat($termpath, '/', 1)"/></td>
	<td><xsl:apply-templates select="*[position()=2]"/></td>
	<td><xsl:value-of select="concat($termpath, '/', 2)"/></td>
	</tr>
	<xsl:apply-templates select="*[position() = 8]" >
		<xsl:with-param name="termpath">
			<xsl:value-of select="concat($termpath, '/', 8)"/>
		</xsl:with-param>
	</xsl:apply-templates>
	<tr>
	<td><xsl:apply-templates select="*[position()=3]"/></td>
	<td><xsl:value-of select="concat($termpath, '/', 3)"/></td>
	<td><xsl:apply-templates select="*[position()=4]"/></td>
	<td><xsl:value-of select="concat($termpath, '/', 4)"/></td>
	</tr>
	<xsl:apply-templates select="*[position() = 9]" >
		<xsl:with-param name="termpath">
			<xsl:value-of select="concat($termpath, '/', 9)"/>
		</xsl:with-param>
	</xsl:apply-templates>
	<tr>
	<td><xsl:apply-templates select="*[position()=5]"/></td>
	<td><xsl:value-of select="concat($termpath, '/', 5)"/></td>
	<td><xsl:apply-templates select="*[position()=6]"/></td>
	<td><xsl:value-of select="concat($termpath, '/', 6)"/></td>
	</tr>
	<xsl:apply-templates select="*[position() = 10]" >
		<xsl:with-param name="termpath">
			<xsl:value-of select="concat($termpath, '/', 10)"/>
		</xsl:with-param>
	</xsl:apply-templates>
</xsl:template>

<xsl:template match="String" priority="50">
	<xsl:param name="termpath" />
	<xsl:text>&quot;</xsl:text>
	<xsl:value-of select="." disable-output-escaping="yes" />
	<xsl:text>&quot;</xsl:text>
	<xsl:if test="following-sibling::* and not(parent::two) and not(parent::three) and not(parent::four)">
		<xsl:text>,</xsl:text>
	</xsl:if>
</xsl:template>
<xsl:template match="Int" priority="50">
	<xsl:param name="termpath" />
	<xsl:value-of select="." disable-output-escaping="yes" />
	<xsl:if test="following-sibling::* and not(parent::two) and not(parent::three) and not(parent::four)">
		<xsl:text>,</xsl:text>
	</xsl:if>
</xsl:template>
<xsl:template match="Float" priority="50">
	<xsl:param name="termpath" />
	<xsl:value-of select="." disable-output-escaping="yes" />
	<xsl:if test="following-sibling::* and not(parent::two) and not(parent::three) and not(parent::four)">
		<xsl:text>,</xsl:text>
	</xsl:if>
</xsl:template>
<xsl:template match="Char" priority="50">
	<xsl:param name="termpath" />
	<xsl:text>(&apos;</xsl:text>
	<xsl:value-of select="." disable-output-escaping="yes" />
	<xsl:text>&apos;)</xsl:text>
	<xsl:if test="following-sibling::* and not(parent::two) and not(parent::three) and not(parent::four)">
		<xsl:text>,</xsl:text>
	</xsl:if>
</xsl:template>

<xsl:template match="*" priority="10">
	<xsl:param name="termpath" />
	<xsl:choose>
	<xsl:when test="@functor">
		<xsl:value-of select="@functor" disable-output-escaping="yes" />
	</xsl:when>
	<xsl:otherwise>
		<xsl:value-of select="name()" disable-output-escaping="yes" />
	</xsl:otherwise>
	</xsl:choose>
	<xsl:if test="count(child::*) != 0">
		<xsl:text>(</xsl:text>
		<xsl:apply-templates />
		<xsl:text>)</xsl:text>
	</xsl:if>
	<xsl:if test="following-sibling::* and not(parent::two) and not(parent::three) and not(parent::four)">
		<xsl:text>,</xsl:text>
	</xsl:if>
</xsl:template>

<xsl:template name="showpath">
	<xsl:param name="termpath" />
	<xsl:choose>
	<xsl:when test="ancestor::*">
		<xsl:value-of select="concat($termpath, '/', position())"/>
	</xsl:when>
	<xsl:otherwise>
		<xsl:value-of select="'/'" />
	</xsl:otherwise>
	</xsl:choose>
</xsl:template>

</xsl:stylesheet>
