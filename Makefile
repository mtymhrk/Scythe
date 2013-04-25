SRC_DIR = src
TEST_DIR = test
RAKE=rake1.9

all: bin

.PHONY: bin unit clean

bin:
	$(MAKE) -C $(SRC_DIR)

unit:
	$(MAKE) -C $(SRC_DIR) test_build CMACROS=-DSCM_UNIT_TEST
	cd $(TEST_DIR); $(RAKE) unit summary

clean:
	$(MAKE) -C $(SRC_DIR) clean
	cd $(TEST_DIR); $(RAKE) clean_all
