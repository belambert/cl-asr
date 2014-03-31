

all: clean doc	

clean:
	echo "Deleting FASL files..."
	find src -name "*.fasl" -exec rm '{}' \;

doc:
	echo "Generating documentation..."
	cd doc; \
	./delete-doc.sh; \
	./gen-doc.sh; \
	cd ..

.PHONY: clean doc