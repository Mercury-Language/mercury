#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#

MMC_DIR=$(shell dirname `which mmc`)
MMC_JARS_DIR=$(MMC_DIR)/../lib/mercury/lib/java
WEBINF=war/WEB-INF

.PHONY: servlet.jar
servlet.jar: wardirs
	mmc --grade java --make libservlet
	mv servlet.jar $(WEBINF)/lib/

.PHONY: wardirs
wardirs:
	mkdir -p $(WEBINF)/lib
	mkdir -p $(WEBINF)/classes

.PHONY: install_mer_jars
install_mer_jars: wardirs
	cp $(MMC_JARS_DIR)/mer_std.jar $(WEBINF)/lib/
	cp $(MMC_JARS_DIR)/mer_rt.jar $(WEBINF)/lib/

.PHONY: run
run: install_mer_jars servlet.jar
	dev_appserver.sh war

.PHONY: deploy
deploy: install_mer_jars servlet.jar
	appcfg.sh update war
