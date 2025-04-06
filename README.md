# 🎬 MOVIES Program

System developed to manage a movie catalog via terminal, with basic operations such as register, search, update, delete, and list.

## 📌 Objective

This program aims to **manage the data of a movie catalog**, allowing the user to interact through an intuitive **options menu**.

---

## 🗂️ File Structure

### 📄 Input/Output File: `ARQFIL`  
Format: `LRECL = 63`

| Field                | Description               | Type     | Size |
|----------------------|---------------------------|----------|------|
| `MOVIES-KEY`         | Movie code                | Numeric  | 005  |
| `MOVIES-TITLE`       | Movie title               | String   | 030  |
| `MOVIES-GENRE`       | Movie genre               | String   | 008  |
| `MOVIES-DURATION`    | Duration (in minutes)     | Numeric  | 003  |
| `MOVIES-DISTRIBUTOR` | Movie distributor         | String   | 015  |
| `MOVIES-RATING`      | Personal rating assigned  | Numeric  | 002  |

---

## ⚙️ Features

### 📋 Options Menu

The system displays a menu with the following options:

1. Register  
2. Search  
3. Update  
4. Delete  
5. Report on Screen  
6. Report on Disk  
7. Exit  

---

### ➕ Register

- Checks if the movie already exists (duplicates are not allowed).
- Asks for confirmation before saving:
  - If confirmed: displays **"Registered entity"**.
  - Otherwise: displays **"Error while registering entity"**.

---

### 🔍 Search

- Displays the data of the searched movie.
- If not found: shows **"Entity not found"**.

---

### ✏️ Update

- Displays movie data for editing (except the code).
- Asks for confirmation:
  - If confirmed: displays **"Entity updated successfully"**.
  - Otherwise: displays **"Error while updating entity"**.

---

### ❌ Delete

- Locates the movie and asks for deletion confirmation:
  - If confirmed: displays **"Successfully deleted entity"**.
  - Otherwise: displays **"Error while deleting entity"**.

---

### 📑 Report on Screen

- Displays all registered movies on the screen.
- Paginated display with 5 records per page.
- Pressing **Enter** shows the next page.

### 💾 Report on Disk

- Generates a report file with all registered movies.

---

## 👨‍🏫 Instructor

**Ivan Petrucci**

## 🧑‍💻 Author

**Alexandre Pedro**
