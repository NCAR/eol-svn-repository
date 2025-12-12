import os

# --- CONFIGURATION ---
REPO_PATH = "."  # Current directory
OUTPUT_FILE = "REPO_MAP.md"
SKIP_DIRS = {'.git', '.idea', '__pycache__', 'node_modules', '.DS_Store', 'dist', 'build'}
SKIP_EXTENSIONS = {'.pyc', '.png', '.jpg', '.svg', '.ico'} # Skip assets if you want only code

def generate_markdown_map():
    with open(OUTPUT_FILE, 'w', encoding='utf-8') as md:
        # Write Title
        repo_name = os.path.basename(os.path.abspath(REPO_PATH))
        md.write(f"# Code Map: {repo_name}\n\n")
        md.write("> Auto-generated structure. Add descriptions manually.\n\n")

        # Walk the tree
        for root, dirs, files in os.walk(REPO_PATH):
            # Sort to keep output consistent
            dirs.sort()
            files.sort()

            # Filter out skipped directories
            dirs[:] = [d for d in dirs if d not in SKIP_DIRS]
            
            # Filter files (optional: skip images/assets)
            visible_files = [f for f in files if os.path.splitext(f)[1] not in SKIP_EXTENSIONS]

            if not visible_files:
                continue

            # Calculate relative path for the Header
            rel_path = os.path.relpath(root, REPO_PATH)
            if rel_path == ".":
                header = "Root Directory"
            else:
                header = rel_path.replace(os.sep, "/")

            # Write Folder Header
            md.write(f"## 📂 {header}\n")
            
            # Write Table Header
            md.write("| File | Description | Tags |\n")
            md.write("| :--- | :--- | :--- |\n")

            # Write File Rows
            for f in visible_files:
                # We put the filename in backticks ` ` for formatting
                md.write(f"| `{f}` |  |  |\n")
            
            md.write("\n") # Spacing between tables

    print(f"Success! Generated {OUTPUT_FILE}. You can now open it and start labeling.")

if __name__ == "__main__":
    generate_markdown_map()
