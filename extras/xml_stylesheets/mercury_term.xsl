<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns="http://www.w3.org/TR/xhtml1/strict">
<!-- 
	This template produces a Mercury term from an xml document 
	generated using term_to_xml.write_xml_doc/6.  The term is suitable for 
	reading into a Mercury program using io.read/3.
-->
<xsl:output method="text" indent="no" media-type="text/plain" />
<xsl:template match="text()" priority="100">
</xsl:template>
<xsl:template match="/" priority="200">
	<xsl:apply-templates />
	<xsl:text>. </xsl:text>
</xsl:template>
<xsl:template match="String" priority="50">
	<xsl:text>&quot;</xsl:text>
	<xsl:value-of select="." disable-output-escaping="yes" />
	<xsl:text>&quot;</xsl:text>
	<xsl:if test="following-sibling::*">
		<xsl:text>,</xsl:text>
	</xsl:if>
</xsl:template>
<xsl:template match="Int" priority="50">
	<xsl:value-of select="." disable-output-escaping="yes" />
	<xsl:if test="following-sibling::*">
		<xsl:text>,</xsl:text>
	</xsl:if>
</xsl:template>
<xsl:template match="Float" priority="50">
	<xsl:value-of select="." disable-output-escaping="yes" />
	<xsl:if test="following-sibling::*">
		<xsl:text>,</xsl:text>
	</xsl:if>
</xsl:template>
<xsl:template match="Char" priority="50">
	<xsl:text>(&apos;</xsl:text>
	<xsl:value-of select="." disable-output-escaping="yes" />
	<xsl:text>&apos;)</xsl:text>
	<xsl:if test="following-sibling::*">
		<xsl:text>,</xsl:text>
	</xsl:if>
</xsl:template>
<xsl:template match="*" priority="10">
	<xsl:choose>
	<xsl:when test="@functor">
		<xsl:text>&apos;</xsl:text>
		<xsl:value-of select="@functor" disable-output-escaping="yes" />
		<xsl:text>&apos;</xsl:text>
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
	<xsl:if test="following-sibling::*">
		<xsl:text>,</xsl:text>
	</xsl:if>
</xsl:template>
</xsl:stylesheet>
