# Municipality Information Management System

A comprehensive Shiny application for managing South African municipality data with interactive editing capabilities, real-time statistics, and database integration.

## 🏛️ Features

### 📊 **Interactive Data Management**
- **Editable Table**: Double-click any cell to edit municipality information
- **Real-time Change Tracking**: Visual highlighting of modified rows until saved
- **Advanced Filtering**: Filter by Province, Municipality Type, or search by name
- **Bulk Operations**: Save multiple changes simultaneously to the database

### 📈 **Statistics Dashboard**
- **Value Boxes**: Quick overview of total municipalities, provinces, and unsaved changes
- **Interactive Charts**: Visual breakdown of municipalities by province and type
- **Detailed Summary**: Comprehensive statistics with current view breakdown

### 💾 **Data Export & Management**
- **Multiple Export Formats**: Download as CSV or Excel
- **Database Operations**: Refresh data from database, create backups
- **Real-time Sync**: Live connection to PostgreSQL database

## 🚀 Quick Start

### Prerequisites
- R (>= 4.0.0)
- PostgreSQL database
- Required R packages (see Installation section)

### Installation

1. **Clone or download** this repository
2. **Install required packages**:
```r
install.packages(c(
  "shiny",
  "DT", 
  "dplyr",
  "DBI",
  "RPostgres",
  "config",
  "here",
  "shinydashboard",
  "openxlsx"
))
```

3. **Set up your database configuration** in `config.yml`:
```yaml
QA:
  MUNbdName: "your_database_name"
  MUNdbHost: "your_host"
  MUNdbPort: 5432
  dbUser: "your_username"
  dbPassword: "your_password"
```

4. **Launch the application**:
```r
shiny::runApp("app.R")
```

## 📋 Database Setup

### Table Structure
The application expects a PostgreSQL table named `mun_info` with the following structure:

| Column | Type | Description |
|--------|------|-------------|
| No. | integer | Primary key (auto-increment) |
| DemarcationCode | varchar | Municipality demarcation code |
| URL | varchar | Municipality website URL |
| Site | varchar | Site identifier |
| WebAPI | varchar | Web API endpoint |
| DemarcationGUID | varchar | Unique identifier |
| Type | varchar | Municipality type (LM/MET/DM) |
| NameOfMunicipality | varchar | Full municipality name |
| Province | varchar | Province code (WC/EC/GP/etc.) |
| District | varchar | District name |
| MuncipalityCentroidLat | varchar | Latitude coordinate |
| MuncipalityCentroidLong | varchar | Longitude coordinate |
| EconomicCenterLat | varchar | Economic center latitude |
| EconomicCenterLong | varchar | Economic center longitude |

### Sample Data
The system supports 266 municipalities across South Africa's 9 provinces with various municipality types:
- **LM**: Local Municipality
- **MET**: Metropolitan Municipality  
- **DM**: District Municipality

## 🎯 Usage Guide

### 1. **Data Tab - Main Interface**
- **View Data**: Browse all municipality information in an interactive table
- **Filter Data**: Use province, type, and text search filters
- **Edit Cells**: Double-click any cell (except No. column) to edit values
- **Track Changes**: Modified rows appear with yellow highlighting
- **Save Changes**: Click "Save Changes" to commit all modifications to database

### 2. **Statistics Tab - Analytics**
- **Overview Cards**: See totals for municipalities, provinces, and unsaved changes
- **Province Chart**: Bar chart showing municipality distribution by province
- **Type Chart**: Pie chart showing breakdown by municipality type
- **Detailed Summary**: Text summary with current filtering statistics

### 3. **Export Tab - Data Management**
- **Download CSV**: Export current data as comma-separated values
- **Download Excel**: Export as Excel spreadsheet (.xlsx)
- **Refresh Data**: Reload data from database (discards unsaved changes)
- **Create Backup**: Database backup functionality

## ⚙️ Configuration

### Environment Setup
Set your R environment configuration:
```r
Sys.setenv(R_CONFIG_ACTIVE = "QA")  # or "PROD", "local"
```

## 🔧 Technical Details

### Architecture
- **Frontend**: Shiny Dashboard with responsive Bootstrap UI
- **Backend**: R server with reactive programming
- **Database**: PostgreSQL with connection pooling
- **Data Processing**: dplyr for data manipulation, DT for interactive tables

### Key Components
- **Reactive Values**: Track data state and changes
- **Database Connection**: Secure PostgreSQL connectivity with proper connection handling
- **Error Handling**: Comprehensive try-catch blocks for robust operation
- **Real-time Updates**: Live synchronization between UI and database

### Performance Features
- **Efficient Queries**: Optimized SQL with proper indexing
- **Connection Management**: Automatic connection cleanup
- **Memory Management**: Efficient data loading and caching
- **User Experience**: Non-blocking operations with progress indicators

## 🛡️ Security Features

- **SQL Injection Protection**: Parameterized queries
- **Connection Security**: Encrypted database connections
- **Access Control**: Environment-based configuration
- **Data Validation**: Input sanitization and type checking

## 🐛 Troubleshooting

### Common Issues

**1. Database Connection Error**
```
Error loading data: could not connect to server
```
**Solution**: Check your `config.yml` settings and ensure PostgreSQL is running.

**2. Table Not Found**
```
Error: relation "mun_info" does not exist
```
**Solution**: Run the database initialization script first:
```r
source("write_to_municipality_info.R")
```

**3. Package Loading Issues**
```
Error: package 'shinydashboard' not found
```
**Solution**: Install missing packages:
```r
install.packages("shinydashboard")
```

### Debug Mode
Enable verbose logging by setting:
```r
options(shiny.trace = TRUE)
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📋 Changelog

### Version 1.0.0
- ✅ Initial release with full CRUD functionality
- ✅ Interactive data table with editing capabilities
- ✅ Real-time statistics dashboard
- ✅ Export functionality (CSV/Excel)
- ✅ Database integration with PostgreSQL
- ✅ Comprehensive error handling


## 🙋‍♂️ Support

For support, please open an issue in the repository or contact the development team.

---

**Built with ❤️ using vibe cding and sheer determination**
