/** \file lispio.h
 * definitions of pure input output classes.
 */


#ifndef YACAS_LISPIO_H
#define YACAS_LISPIO_H

#include <cstddef>
#include <string>

class InputStatus
{
public:
  InputStatus() : iFileName("none") , iLineNumber(-1)  {}

  void SetTo(const std::string& aFileName);
  void RestoreFrom(InputStatus& aPreviousStatus);
  int LineNumber() const;
  const std::string& FileName() const;
  void NextLine();

private:
  std::string iFileName;
  int  iLineNumber;
};

inline
int InputStatus::LineNumber() const
{
  return iLineNumber;
}

inline
const std::string& InputStatus::FileName() const
{
  return iFileName;
}

inline
void InputStatus::NextLine()
{
  iLineNumber++;
}

/** \class LispInput : pure abstract class declaring the interface
 *  that needs to be implemented by a file (something that expressions
 *  can be read from).
 */
class LispInput
{
public:
  /** Constructor with InputStatus. InputStatus retains the information
   * needed when an error occurred, and the file has already been
   * closed.
   */
  LispInput(InputStatus& aStatus) : iStatus(aStatus) {};
  virtual ~LispInput() = default;

  /// Return the next character in the file
  virtual char32_t Next() = 0;

  /** Peek at the next character in the file, without advancing the file
   *  pointer.
   */
  virtual char32_t Peek() = 0;

  virtual const InputStatus& Status() const {return iStatus;};

  /// Check if the file position is past the end of the file.
  virtual bool EndOfStream() const = 0;

  virtual std::size_t Position() const = 0;
  virtual void SetPosition(std::size_t aPosition) = 0;
protected:
  InputStatus& iStatus;
};

#endif

