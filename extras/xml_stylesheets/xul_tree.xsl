<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<!-- 
	This template produces a XUL term browser for a Mercury term converted
	to XML using the term_to_xml library.  The generated XUL can be viewed
	with a Mozilla or Firefox browser.
-->
<xsl:output method="xml" indent="yes" 
	media-type="application/vnd.mozilla.xul+xml" />
<xsl:strip-space elements="*" />
<xsl:template match="/" priority="200">
	<xsl:element name="window">
		<xsl:attribute name="title">Nodes</xsl:attribute>
		<xsl:attribute name="height">500</xsl:attribute>
		<xsl:attribute name="width">600</xsl:attribute>
		<xsl:attribute name="xmlns">http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul</xsl:attribute>
		<xsl:element name="tree">
			<xsl:attribute name="flex">1</xsl:attribute>
			<xsl:attribute name="enableColumnDrag">true</xsl:attribute>
			<xsl:element name="treecols">
				<xsl:element name="treecol">
					<xsl:attribute name="id">nodes</xsl:attribute>
					<xsl:attribute name="label">Nodes</xsl:attribute>
					<xsl:attribute name="primary">true</xsl:attribute>
					<xsl:attribute name="flex">5</xsl:attribute>
				</xsl:element>
				<xsl:element name="splitter">
					<xsl:attribute name="class">tree-splitter</xsl:attribute>
				</xsl:element>
				<xsl:element name="treecol">
					<xsl:attribute name="id">field</xsl:attribute>
					<xsl:attribute name="label">Field Name</xsl:attribute>
					<xsl:attribute name="flex">1</xsl:attribute>
					<xsl:attribute name="hidden">true</xsl:attribute>
				</xsl:element>
				<xsl:element name="splitter">
					<xsl:attribute name="class">tree-splitter</xsl:attribute>
				</xsl:element>
				<xsl:element name="treecol">
					<xsl:attribute name="id">type</xsl:attribute>
					<xsl:attribute name="label">Type</xsl:attribute>
					<xsl:attribute name="flex">1</xsl:attribute>
				</xsl:element>
				<xsl:element name="splitter">
					<xsl:attribute name="class">tree-splitter</xsl:attribute>
				</xsl:element>
				<xsl:element name="treecol">
					<xsl:attribute name="id">depth</xsl:attribute>
					<xsl:attribute name="label">Depth</xsl:attribute>
					<xsl:attribute name="flex">1</xsl:attribute>
					<xsl:attribute name="hidden">true</xsl:attribute>
				</xsl:element>
				<xsl:element name="splitter">
					<xsl:attribute name="class">tree-splitter</xsl:attribute>
				</xsl:element>
				<xsl:element name="treecol">
					<xsl:attribute name="id">termpath</xsl:attribute>
					<xsl:attribute name="label">Term Path</xsl:attribute>
					<xsl:attribute name="flex">1</xsl:attribute>
				</xsl:element>
				<xsl:element name="splitter">
					<xsl:attribute name="class">tree-splitter</xsl:attribute>
				</xsl:element>
				<xsl:element name="treecol">
					<xsl:attribute name="id">fieldtermpath</xsl:attribute>
					<xsl:attribute name="label">Term Path (with field names)</xsl:attribute>
					<xsl:attribute name="flex">1</xsl:attribute>
					<xsl:attribute name="hidden">true</xsl:attribute>
				</xsl:element>
			</xsl:element>
			<xsl:element name="treechildren">
				<xsl:apply-templates />
			</xsl:element>
		</xsl:element>
	</xsl:element>
</xsl:template>
<xsl:template match="String|Int|Float|Char" priority="50">
	<xsl:param name="depth">0</xsl:param>
	<xsl:param name="termpath" />
	<xsl:param name="fieldtermpath" />
	<xsl:element name="treeitem">
		<xsl:element name="treerow">
			<xsl:element name="treecell">
				<xsl:choose>
					<xsl:when test="name()='String'">
						<xsl:attribute name="label">&quot;<xsl:value-of select="." />&quot;</xsl:attribute>
					</xsl:when>
					<xsl:otherwise>
						<xsl:attribute name="label"><xsl:value-of select="." /></xsl:attribute>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:element>
			<xsl:element name="treecell">
				<xsl:attribute name="label">
					<xsl:value-of select="@field" />
				</xsl:attribute>
			</xsl:element>
			<xsl:element name="treecell">
				<xsl:attribute name="label"><xsl:value-of select="@type" /></xsl:attribute>
			</xsl:element>
			<xsl:element name="treecell">
				<xsl:attribute name="label">
					<xsl:value-of select="$depth" />
				</xsl:attribute>
			</xsl:element>
			<xsl:element name="treecell">
				<xsl:attribute name="label">
					<xsl:call-template name="showpath">
						<xsl:with-param name="termpath"><xsl:value-of select="$termpath" /></xsl:with-param>
					</xsl:call-template>
				</xsl:attribute>
			</xsl:element>
			<xsl:element name="treecell">
				<xsl:attribute name="label">
					<xsl:call-template name="showfieldpath">
						<xsl:with-param name="fieldtermpath"><xsl:value-of select="$fieldtermpath" /></xsl:with-param>
					</xsl:call-template>
				</xsl:attribute>
			</xsl:element>
		</xsl:element>
	</xsl:element>
</xsl:template>
<xsl:template match="*" priority="10">
	<xsl:param name="depth">0</xsl:param>
	<xsl:param name="termpath" />
	<xsl:param name="fieldtermpath" />
	<xsl:choose>
	<xsl:when test="parent::*[@functor='[|]'] and @functor='[|]'">
		<xsl:if test="count(child::*) != 0">
			<xsl:apply-templates>
				<xsl:with-param name="depth" select="$depth+1" />
				<xsl:with-param name="termpath">
					<xsl:if test="ancestor::*">
						<xsl:value-of select="concat($termpath, '/', position())"/>
					</xsl:if>
				</xsl:with-param>
			</xsl:apply-templates>
		</xsl:if>
	</xsl:when>
	<xsl:otherwise>
	<xsl:element name="treeitem">
		<xsl:if test="count(child::*) != 0">
			<xsl:attribute name="container">true</xsl:attribute>
		</xsl:if>
		<xsl:element name="treerow">
			<xsl:element name="treecell">
				<xsl:attribute name="label">
					<xsl:choose>
					<xsl:when test="@functor">
						<xsl:value-of select="@functor" />
					</xsl:when>
					<xsl:otherwise>
						<xsl:value-of select="name()" />
					</xsl:otherwise>
					</xsl:choose>
				</xsl:attribute>
			</xsl:element>
			<xsl:element name="treecell">
				<xsl:attribute name="label">
					<xsl:value-of select="@field" />
				</xsl:attribute>
			</xsl:element>
			<xsl:element name="treecell">
				<xsl:attribute name="label">
					<xsl:value-of select="@type" />
				</xsl:attribute>
			</xsl:element>
			<xsl:element name="treecell">
				<xsl:attribute name="label">
					<xsl:value-of select="$depth" />
				</xsl:attribute>
			</xsl:element>
			<xsl:element name="treecell">
				<xsl:attribute name="label">
					<xsl:call-template name="showpath">
						<xsl:with-param name="termpath"><xsl:value-of select="$termpath" /></xsl:with-param>
					</xsl:call-template>
				</xsl:attribute>
			</xsl:element>
			<xsl:element name="treecell">
				<xsl:attribute name="label">
					<xsl:call-template name="showfieldpath">
						<xsl:with-param name="fieldtermpath"><xsl:value-of select="$fieldtermpath" /></xsl:with-param>
					</xsl:call-template>
				</xsl:attribute>
			</xsl:element>
		</xsl:element>
		<xsl:if test="count(child::*) != 0">
			<xsl:element name="treechildren">
				<xsl:apply-templates>
					<xsl:with-param name="depth" select="$depth+1" />
					<xsl:with-param name="termpath">
						<xsl:if test="ancestor::*">
							<xsl:value-of select="concat($termpath, '/', position())"/>
						</xsl:if>
					</xsl:with-param>
					<xsl:with-param name="fieldtermpath">
						<xsl:if test="ancestor::*">
							<xsl:choose>
							<xsl:when test="@field">
								<xsl:value-of select="concat($fieldtermpath, '/', @field)"/>
							</xsl:when>
							<xsl:otherwise>
								<xsl:value-of select="concat($fieldtermpath, '/', position())"/>
							</xsl:otherwise>
							</xsl:choose>
						</xsl:if>
					</xsl:with-param>
				</xsl:apply-templates>
			</xsl:element>
		</xsl:if>
	</xsl:element>
	</xsl:otherwise>
	</xsl:choose>
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
<xsl:template name="showfieldpath">
	<xsl:param name="termpath" />
	<xsl:choose>
	<xsl:when test="ancestor::*">
		<xsl:choose>
		<xsl:when test="@field">
			<xsl:value-of select="concat($fieldtermpath, '/', @field)"/>
		</xsl:when>
		<xsl:otherwise>
			<xsl:value-of select="concat($fieldtermpath, '/', position())"/>
		</xsl:otherwise>
		</xsl:choose>
	</xsl:when>
	<xsl:otherwise>
		<xsl:value-of select="'/'" />
	</xsl:otherwise>
	</xsl:choose>
</xsl:template>
</xsl:stylesheet>
